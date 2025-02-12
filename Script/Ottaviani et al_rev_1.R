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
               "gam", # Generalized Additive Models 
               "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
               "parameters", # Processing of Model Parameters
               "performance", # Assessment of Regression Models Performance
               "PupillometryR", # A Unified Pipeline for Pupillometry Data
               "readxl", # Read Excel Files
               "tidyr") # Tidy Messy Data

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

# Custom theme for ggplot2
theme_ggplot <- theme(
  legend.position = "bottom",
  axis.title = element_text(size = 12),
  axis.text.x = element_text(size = 11),
  axis.text.y = element_text(size = 11),
  panel.grid = element_blank(),
  plot.caption = element_text(size = 10, color = "gray50"),
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

#Run from here (takes approxmately 2 minutes):
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
  
  # #listing the words used
  # word_abstract <- c(abstract_i[abstract_i %in% novelty == TRUE], abstract_i[abstract_i %in% confirmatory == TRUE])
  # 
  # year_abstract <- append(year_abstract, rep(db$PY[i],length(word_abstract)))
  # 
  # used_word_abstract <- append(used_word_abstract,word_abstract)
  
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
par(mfrow = c(2,2))
hist(db$novelty_abstract_ratio, breaks = 30, main = "abstract (novelty)")
hist(db$confirmatory_abstract_ratio, breaks = 30, main = "abstract (confirm.)")
hist(db$novelty_title_ratio, breaks = 30, main = "title (novelty)") #we cannot really use the title
hist(db$confirmatory_title_ratio, breaks = 30, main = "title (confirm.)") #we cannot really use the title

# Is there an effect of using the two types of term on citations and impact factor?
db$Novelty_AB_f      <- as.factor(ifelse(db$novelty_abstract>0,1,0))
db$Confirmatory_AB_f <- as.factor(ifelse(db$confirmatory_abstract>0,1,0))
db$Novelty_TI_f      <- as.factor(ifelse(db$novelty_title>0,1,0))
db$Confirmatory_TI_f <- as.factor(ifelse(db$confirmatory_title>0,1,0))

# levels(db$Novelty_AB_f)      <- c("Not used","Used")
# levels(db$Confirmatory_AB_f) <- c("Not used","Used")
# 
# levels(db$Novelty_TI_f)      <- c("Not used","Used")
# levels(db$Confirmatory_TI_f) <- c("Not used","Used")

# Looking at trends -------------------------------------------------------

#Preparting the data
db_year <- db %>% group_by(PY) %>% 
  summarise(SUM_AB_NOVEL = (sum(ifelse(novelty_abstract>0,1,0))/length(PY) )* 100,
            SUM_AB_CONFIRM = (sum(ifelse(confirmatory_abstract>0,1,0))/length(PY) ) * 100,
            SUM_TI_NOVEL = (sum(ifelse(novelty_title>0,1,0))/length(PY) )* 100,
            SUM_TI_CONFIRM = (sum(ifelse(confirmatory_title>0,1,0))/length(PY) ) * 100 )

# Is there a temporal trend?

M1 <- glm(Novelty_AB_f ~ PY, data = db, family = "binomial")
(pM1 <- parameters::model_parameters(M1))

M2 <- glm(Confirmatory_AB_f ~ PY, data = db, family = "binomial")
(pM2 <- parameters::model_parameters(M2))

# Is there a temporal trend?
M1 <- lm(SUM_AB_NOVEL ~ PY, data = db_year)
(pM1 <- parameters::model_parameters(M1))

M2 <- lm(SUM_AB_CONFIRM ~ PY, data = db_year)
(pM2 <- parameters::model_parameters(M2))

# Model summary for plot --------------------------------------------------

label_p1 <- paste("LM: beta=",
                  round(pM1$Coefficient[2],2),
                  "; 95% CI=[",
                  round(pM1$CI_low[2],2),
                  ",",
                  round(pM1$CI_high[2],2),
                  "]; p",
                  ifelse(pM1$p[2]<0.001,"<0.001",paste("=",round(pM1$p[2],3),sep='')), 
                  sep=''   )

label_p2 <- paste("LM: beta=",
                  round(pM2$Coefficient[2],2),
                  "; 95% CI=[",
                  round(pM2$CI_low[2],2),
                  ",",
                  round(pM2$CI_high[2],2),
                  "]; p",
                  ifelse(pM2$p[2]<0.001,"<0.001",paste("=",round(pM2$p[2],3),sep='')), 
                  sep=''   )

# Generating the plots ----------------------------------------------------

db_year2 <- data.frame(PY     = rep(db_year$PY,2),
                      SUM_AB = c(db_year$SUM_AB_NOVEL, db_year$SUM_AB_CONFIRM),
                      TYPE   =   c(rep("Novel",nrow(db_year)),
                                   rep("Confirmatory  ",nrow(db_year)))) #space for the legend

(plot <- ggplot(db_year2, aes(x = PY, y = SUM_AB, colour=TYPE, fill=TYPE)) + 
        labs(title = NULL, x = NULL, 
       y = "% of papers")+
       geom_smooth(method = "lm", formula = y ~ x, alpha =.5 )+
       geom_point(pch= 21, col = "grey10", size = 2.5) +
       ylim(0,22)+
       scale_color_manual("",values = c("midnightblue","mediumorchid4"))+
       scale_fill_manual("",values = c("midnightblue","mediumorchid4"))+
       annotate("text", x = 2006, y = 20, label = label_p1, col= "mediumorchid4") +
       annotate("text", x = 2006, y = 6, label = label_p2, col = "midnightblue") +
      scale_x_continuous(breaks = c(seq(from=1997,to=2017,by=3)), 
                            labels = c(seq(from=1997,to=2017,by=3))) + 
         theme_classic() + theme_ggplot)

ggsave("Figures/FIGURE_1.pdf", plot)

# Further analyses --------------------------------------------------------

#Normalizing citations by article age using a GAM

# Approach as in:
# Mammola, S., Fontaneto, D., Martínez, A., & Chichorro, F. (2021). Impact of the reference list features on the number of citations. Scientometrics, 126(1), 785-799.

#Calculating article age
db$Article_age <- 2017 - db$PY

#removing NA
db <- db[!is.na(db$TC), ]
db <- db[!is.na(db$Article_age), ]

#Checking outliers
boxplot(db$TC) # 3 outliers
db <- db[db$TC<6000,]

#fitting the gam
M0 <- gam::gam(TC ~ s(Article_age,2), family = poisson, data = db) 
summary(M0)

par(mfrow=c(1,1)) ; plot(M0, se = FALSE)

# Extracting the residual of citations
db <- data.frame(db, Citation_residuals = resid(M0, type="pearson"))

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

#Omitting NA in IF
db_NA_omit <- db %>% drop_na(IF)

# levels(db$Novelty_AB_f)      <- c("Not used","Used")
# levels(db$Confirmatory_AB_f) <- c("Not used","Used")


M1 <- lme4::lmer(IF ~ Novelty_AB_f + Confirmatory_AB_f + PY + n_abstract + (1|J9), data = db, REML= FALSE)
(pM1 <- parameters::model_parameters(M1))

#Validaton plots
performance::check_model(M1)
performance::r2(M1)

M2 <- lme4::lmer(TC ~ Novelty_AB_f + Confirmatory_AB_f + PY + n_abstract + (1|J9), data = db, REML= FALSE)
(pM2 <- parameters::model_parameters(M2))

#Validaton plots
performance::check_model(M2)
performance::r2(M2)
