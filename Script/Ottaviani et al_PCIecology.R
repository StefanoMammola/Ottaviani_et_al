## ------------------------------------------------------------------------
## 'On the quest of novelty in ecological research'
## ------------------------------------------------------------------------

## Gianluigi Ottaviani, Alejandro Martínez, Matteo Petit Bon, Stefano Mammola

## ------------------------------------------------------------------------
# 'R script to prepare the data
## ------------------------------------------------------------------------

# Analysis performed with R (v. R 4.0.3) and R studio (v. 1.4.1103)
# Author: Stefano Mammola

# Loading R packages ------------------------------------------------------

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load("dplyr", # A Grammar of Data Manipulation
               "glmmTMB", # Generalized Additive Models 
               "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
               "parameters", # Processing of Model Parameters
               "performance", # Assessment of Regression Models Performance
               "PupillometryR", # A Unified Pipeline for Pupillometry Data
               "readxl", # Read Excel Files
               "tidyr") # Tidy Messy Data

library("glmmTMB")

# Loading useful functions ------------------------------------------------

# Function to clean string of words and abstracts 
# Re-adapted from:
# Martinez, A., & Mammola, S. (2021) Specialized terminology reduces the number of citations of scientific papers. Proceedings of the Royal Society B, 288(1948), 20202581.

word.cleaner <- function(word.list, remove.punctuation = FALSE, split = FALSE, split.sep = " ") {

  if(split)
    word.list <- strsplit(word.list, split.sep)[[1]]
  
  word.list <- tolower(as.vector(word.list))
  
  if(remove.punctuation)
    word.list <- gsub('[[:punct:] ]+','',word.list)
  
  return(word.list)
  
}

# Function to check whole numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


# Custom theme for ggplot2 ---------------------------------------------

theme_ggplot <- theme(
  legend.position = "top",
  axis.title = element_text(size = 14),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  panel.grid = element_blank(),
  plot.caption = element_text(size = 12, color = "gray50"),
  plot.title = element_text(face="bold", size=12)
)

# Loading the database ----------------------------------------------------

db <- data.frame(read_excel("Data/alldata.xlsx")) 

#downloaded on 24 September 2019 in Helsinki ; Web of Science

# Database is the same used in (see reference for details)
# Mammola, S., Fontaneto, D., Martínez, A., & Chichorro, F. (2021). Impact of the reference list features on the number of citations. Scientometrics, 126(1), 785-799.

str(db)

## Variables explanation:

#db$JI #<----- Journal
#db$SO #<----- Journal (extended)
#db$AB #<----- Abstract
#db$TC #<----- Number of citations
#db$PY #<----- Publication year 

# Information on the other columns:
# https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html

# Converting chr to factor
db$JI <- as.factor(db$JI)

#Subselecting only research articles (i.e. removing review, editorial)
db <- db[db$DT %in% c("Article","Letter","Article; Proceedings Paper"),]

#Removing wrong years and articles with no abstract
nrow(db)
db <- db[db$PY > 1996,]
db <- db[!is.na(db$AB), ]
nrow(db)

# Cleaning abstract and titles --------------------------------------------
Abstract_clean <- lapply(1:length(db$AB), function(x) word.cleaner(db$AB[x], remove.punctuation = TRUE, split = TRUE, split.sep=" "))
Title_clean    <- lapply(1:length(db$TI), function(x) word.cleaner(db$TI[x], remove.punctuation = TRUE, split = TRUE, split.sep=" "))

# Check the first abstract
Abstract_clean[[1]] #All words are separated ; capital letters are removed ; punctuation is removed

# Defining the list of words ----------------------------------------------
novelty      <- c("innovative","innovation","innovated","novelty","novel", "new", "newly", "breakthrough","groundbreaking")
confirmatory <- c("replicate","replicated","replication","replicability","reproducibility", "confirmatory","confirm")

# Matching the list using a loop ------------------------------------------
n_abstract               <- c()
novelty_abstract         <- c()
confirmatory_abstract    <- c()
used_word_abstract       <- c()
year_abstract            <- c()
sample_size              <- nrow(db)

n_title            <- c()
novelty_title      <- c()
confirmatory_title <- c()

#Run from here (takes approximately 2 minutes):
for (i in 1:sample_size){
  
  #Analysing abstract i ---
  abstract_i <- Abstract_clean[[i]]
  
  #calculating the number of words in abstract and title
  n_abstract <- append(n_abstract,length(unique(abstract_i)))
  
  #calculating the number of novelty words in abstract and title
  novelty_abstract      <- append(novelty_abstract, sum(ifelse(abstract_i %in% novelty == T,1,0))) 
  confirmatory_abstract <- append(confirmatory_abstract, sum(ifelse(abstract_i %in% confirmatory == T,1,0))) 
  
  #Analysing title i ---
  title_i <- Title_clean[[i]]
  
  #calculating the number of words in abstract and title
  n_title <- append(n_title,length(unique(title_i)))
  
  #calculating the number of novelty words in abstract and title
  novelty_title       <- append(novelty_title, sum(ifelse(title_i %in% novelty == T,1,0))) 
  confirmatory_title  <- append(confirmatory_title, sum(ifelse(title_i %in% confirmatory == T,1,0))) 

  # Checking the advancing 
  if(is.wholenumber(i/1000) == TRUE)
    message(paste("Analyzed", as.character(i), "papers out of", as.character(sample_size),sep=" "))

}

#Storing the data:
db <- data.frame(db, 
                 novelty_abstract_ratio = novelty_abstract/n_abstract,
                 confirmatory_abstract_ratio = confirmatory_abstract/n_abstract,
                 novelty_abstract = novelty_abstract,
                 confirmatory_abstract = confirmatory_abstract,
                 n_abstract = n_abstract,
                 novelty_title_ratio = novelty_title/n_title,
                 confirmatory_title_ratio = confirmatory_title/n_title,
                 novelty_title = novelty_title,
                 confirmatory_title = confirmatory_title,
                 n_title = n_title)

#Checking the data
par(mfrow = c(2,1))
hist(db$novelty_abstract_ratio, breaks = 30, main = "abstract (novelty)")
hist(db$confirmatory_abstract_ratio, breaks = 30, main = "abstract (confirm.)")

par(mfrow = c(2,1))
hist(db$novelty_title_ratio, breaks = 30, main = "title (novelty)") #we cannot really use the title - too zero inflated
hist(db$confirmatory_title_ratio, breaks = 30, main = "title (confirm.)") #we cannot really use the title - too zero inflated

# Is there a temporal trend? ------------------------------------------------
db$Novelty_AB_f      <- as.factor(ifelse(db$novelty_abstract>0,1,0))
db$Confirmatory_AB_f <- as.factor(ifelse(db$confirmatory_abstract>0,1,0))

M0 <- glmmTMB::glmmTMB(Novelty_AB_f ~ scale(PY) + (1 | J9), data = db, family = "binomial"(link = "cloglog"))
(pM0 <- parameters::model_parameters(M0))
performance::r2(M0)
performance::check_model(M0)

M0.1 <- glmmTMB::glmmTMB(Confirmatory_AB_f ~ scale(PY) + (1 | J9), data = db, family = "binomial"(link = "cloglog"))
(pM0.1 <- parameters::model_parameters(M0.1))
performance::r2(M0.1)
performance::check_model(M0.1)

# Generating temporal trend plots ----------------------------------------------------

#Preparing the data
db_year <- db %>% group_by(PY) %>% 
  summarise(SUM_AB_NOVEL = (sum(ifelse(novelty_abstract>0,1,0))/length(PY) )* 100,
            SUM_AB_CONFIRM = (sum(ifelse(confirmatory_abstract>0,1,0))/length(PY) ) * 100,
            COUNT = n())

db_year <- data.frame(PY     = rep(db_year$PY,2),
                       COUNT = rep(db_year$COUNT,2),
                      SUM_AB = c(db_year$SUM_AB_NOVEL, db_year$SUM_AB_CONFIRM),
                      TYPE   =   c(rep("Novel",nrow(db_year)),
                                   rep("Confirmatory  ",nrow(db_year)))) #space for the legend

(plot <- ggplot(db_year, aes(x = PY, y = SUM_AB, colour=TYPE, fill=TYPE, size = COUNT)) + 
        labs(title = NULL, x = NULL, 
       y = "% of papers")+
       geom_line(size = 1)+
       geom_smooth(method = "lm", formula = y ~ x, alpha =.5, size = 1)+
       geom_point(pch= 21, col = "grey10") +
       ylim(0,22)+
       scale_color_manual("Trend:",values = c("midnightblue","mediumorchid4"))+
       scale_fill_manual("Trend:",values = c("midnightblue","mediumorchid4"))+
       scale_size_continuous("Sample size:")+
       scale_x_continuous(breaks = c(seq(from=1997,to=2017,by=3)), 
                            labels = c(seq(from=1997,to=2017,by=3))) + 
         theme_classic() + theme_ggplot)

ggsave("Figures/FIGURE_2.pdf", plot)

db_year2 <- db %>% group_by(PY, SO) %>% 
  summarise(SUM_AB_NOVEL = (sum(ifelse(novelty_abstract>0,1,0))/length(PY) )* 100,
            SUM_AB_CONFIRM = (sum(ifelse(confirmatory_abstract>0,1,0))/length(PY) ) * 100,
            COUNT = n())

db_year2 <- data.frame(PY     = rep(db_year2$PY,2),
                       SO     = rep(db_year2$SO,2),
                       COUNT = rep(db_year2$COUNT,2),
                       SUM_AB = c(db_year2$SUM_AB_NOVEL, db_year2$SUM_AB_CONFIRM),
                       TYPE   =   c(rep("Novel",nrow(db_year2)),
                                    rep("Confirmatory  ",nrow(db_year2))))

# Adding some journal info
db_year2$SO <- as.factor(db_year2$SO)
levels(db_year2$SO)[1] <- "ACTA OECOLOGICA"

#Does the journal mention Novelty in the author guidelines?
journal_info <- data.frame(SO = levels(db_year2$SO), Novelty_in_description = c("No","Yes","Yes","Yes","Yes","Yes","Yes",
                                                                                "No","No","No","Yes","Yes","No","Yes","No","Yes","Yes"))
db_year2 <- db_year2 %>% dplyr::left_join(journal_info, by = "SO")

(plot2 <- ggplot(db_year2, aes(x = PY, y = SUM_AB, 
                               colour=TYPE, 
                               fill=TYPE, pch = Novelty_in_description,
                               size = COUNT)) + 
    facet_wrap(~SO)+
    geom_smooth(method = "lm", formula = y ~ x, alpha =.5, size = 1)+
    labs(title = NULL, x = NULL, 
         y = "% of papers")+
    ylim(0,40)+
    geom_line(size = 1)+
    geom_point(col = "grey10") +
    scale_color_manual("Trend:",values = c("midnightblue","mediumorchid4"))+
    scale_fill_manual("Trend:",values = c("midnightblue","mediumorchid4"))+
    scale_size_continuous("Sample size:")+
    scale_shape_manual("Novelty criteria\nin journal description",values = c(21,22))+
    scale_x_continuous(breaks = c(seq(from=1997,to=2017,by=6)), 
                       labels = c(seq(from=1997,to=2017,by=6))) + 
    theme_classic() + theme_ggplot + theme(legend.box = "horizontal",
                                           legend.position = c(1, 0),
                                           legend.justification = c(1, 0)))



ggsave("Figures/FIGURE_3.pdf", plot2)


#############################################################################
# Testing the relationship between word use and citations and impact factor #
#############################################################################

#Checking outliers
boxplot(db$TC) # 3 outliers
db <- db[db$TC<6000,]

# Assigning Impact factor for each publication ----------------------------

#Uploading the database with all Impact factors between 1997 and 2017
my_files   <- list.files("Data/IF/")
my_files   <- paste("Data/IF/",my_files, sep = '')
all_IF     <- lapply(my_files, read.csv, header = TRUE, sep = ",")

my_IF <- list() ; year_IF <- 1997:2018

#generating a database list all journal and their impact factor for each year
for (k in 1:length(all_IF)){
  
  df <- all_IF[[k]]
  df <- df[,3:5]
  colnames(df) <- c("journal","IF","Immediancy")
  df[df == "Not Available"] <- "NA"
  
  df$IF         <- as.numeric(as.character(df$IF))
  df$Immediancy <- as.numeric(as.character(df$Immediancy))
  df$journal    <- as.character(df$journal)
  df$journal    <- sapply(df$journal,tolower)
  df$journal    <- paste(df$journal, rep(year_IF[[k]],nrow(df)),sep=' ')
  df$journal    <- gsub('[[:punct:] ]+','',df$journal)
  
  df %>% distinct()
  
  my_IF[[k]] <- df
} # ( Warning is due to creation of missing data)

IF_1997_2018 <- rbind(my_IF[[1]],my_IF[[2]],my_IF[[3]],my_IF[[4]],my_IF[[5]],my_IF[[6]],my_IF[[7]],
                      my_IF[[8]],my_IF[[9]],my_IF[[10]],my_IF[[11]],my_IF[[12]],my_IF[[13]],my_IF[[14]],
                      my_IF[[15]],my_IF[[16]],my_IF[[17]],my_IF[[18]],my_IF[[19]],my_IF[[20]],my_IF[[21]],my_IF[[22]])

colnames(IF_1997_2018) <- c("JI_PY","IF","Immediancy")

db$JI_PY <- paste(word.cleaner(db$JI,remove.punctuation = TRUE), db$PY,sep='')

db <- merge(x = db, y = IF_1997_2018, by = "JI_PY", all.x = TRUE, all.y = FALSE, sort = FALSE, no.dups = FALSE, incomparables = NA) 

rm(my_files,all_IF,my_IF,df,IF_1997_2018) #cleaning

# Omitting NA in IF
db_NA_omit <- db %>% drop_na(IF)

# Modelling ---------------------------------------------------------------

levels(db$Novelty_AB_f)      <- c("Not used","Used")
levels(db$Confirmatory_AB_f) <- c("Not used","Used")

M1 <- lm(IF ~ Novelty_AB_f + Confirmatory_AB_f + scale(PY) + scale(n_abstract), data = db)
(pM1 <- parameters::model_parameters(M1))


#Validation plots
performance::check_model(M1)
(M1.R2 <- performance::r2(M1))

M2 <- glmmTMB::glmmTMB(TC ~ Novelty_AB_f + Confirmatory_AB_f + scale(PY) + scale(n_abstract) + (1|J9), data = db, family = "poisson")
performance::check_overdispersion(M2) 
# Overdispersion test
# dispersion ratio =      96.513
# Pearson's Chi-Squared = 5040868.516
# p-value =     < 0.001

M2 <- glmmTMB::glmmTMB(TC ~ Novelty_AB_f + Confirmatory_AB_f + scale(PY) + scale(n_abstract) + (1|J9), data = db, family = "nbinom2")
(pM2 <- parameters::model_parameters(M2))

#Validaton plots
performance::check_model(M2)
(M2.R2 <- performance::r2(M2))

# Visualizing the effect ---------------------------------------------------

# Storing and organising model results
table.M1 <- pM1 %>% dplyr::select(Parameter,
                                  Beta = Coefficient,
                                  SE,
                                  CI_low,
                                  CI_high,
                                  p) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, ~ round(.,3))

table.M1$Parameter[c(4,5)] <- c("PY", "n_abstract")

table.M2 <- pM2 %>% dplyr::select(Parameter,
                                  Effects,
                                  Beta = Coefficient,
                                  SE,
                                  CI_low,
                                  CI_high,
                                  p) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, ~ round(.,3)) 

table.M2 <- table.M2[table.M2$Effects == "fixed",] %>% 
  dplyr::select(-c(Effects)) %>% 
  na.omit()

table.M2$Parameter[c(4,5)] <- c("PY", "n_abstract")

# What is the correlation between Scientific and societal interest estimates?
table.M <- cbind(Model = c(rep("Impact Factor",nrow(table.M1)),
                           rep("Citations", nrow(table.M2))),
                 rbind(table.M1,table.M2)) ; rm(table.M1,table.M2)

table.M$Parameter <- as.factor(as.character(table.M$Parameter))
table.M$Model     <- as.factor(as.character(table.M$Model))

table.M <- table.M[table.M$Parameter != "(Intercept)",] ; table.M = droplevels(table.M)

levels(table.M$Parameter) <- c("Confirmatory terms used [yes]", "Abstract length", "Novelty terms used [yes]","Publication year")

table.M$Parameter <- factor(table.M$Parameter, rev(c("Novelty terms used [yes]","Confirmatory terms used [yes]","Abstract length", "Publication year"))) #Sort

levels(table.M$Model) <- c("Number of citations", "Journal Impact Factor")

sign.M1.2 <- ifelse(table.M$p > 0.05, "", ifelse(table.M$p > 0.01,"", " *")) #Significance

# forest plot
(plot3 <- 
    table.M %>%
    ggplot2::ggplot(aes(x = Beta, y = Parameter)) + 
    facet_wrap(. ~ Model, nrow = 1, ncol = 2, scales = "free") +  
    geom_vline(lty = 3, size = 0.5, col = "grey50", xintercept = 0) +
    geom_errorbar(aes(xmin = CI_low, xmax = CI_high), width = 0)+
    geom_point(col = "grey10", fill = "grey20", size = 2, pch = 21) +
    geom_text(label = paste0(round(table.M$Beta, 2), sign.M1.2, sep = "  "), 
              vjust = - 1, size = 3) +
    labs(x = expression(paste("Estimated beta" %+-% "95% Confidence interval")),
         y = NULL,
         caption = " * p<0.05") +
    
    geom_text(data = data.frame(x = -.2, y = 4.5, Model = "Journal Impact Factor",
                                label = paste0("R^2 ==",round(as.numeric(M1.R2[1]),2))),
              aes(x = x, y = y, label = label),
              size = 3, parse = TRUE) +

    geom_text(data = data.frame(x = -.5, y = 4.5, Model = "Number of citations",
                                label = paste0("Conditional_","R^2 ==",round(as.numeric(M2.R2[1]),2))),
              aes(x = x, y = y, label = label),
              size = 3, parse = TRUE) +
    geom_text(data = data.frame(x = -.5, y = 4.3, Model = "Number of citations",
                                label = paste0(" ","Marginal_","R^2 ==",round(as.numeric(M2.R2[2]),2))),
              aes(x = x, y = y, label = label),
              size = 3, parse = TRUE) +
    
    theme_classic())
    
ggsave("Figures/FIGURE_4.pdf", plot3)    
#End