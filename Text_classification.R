rm(list = ls())
setwd("E:/My documents/Sem-3/D/wd")

##########################################################

library("dplyr")
library("caret")
library("tm")
library("stringr")
library("wordcloud")
library("slam")

##########################################################

data <- read.csv("TextClassification_Data.csv", header = TRUE, na.strings = c(""," "))
str(data)

data$fileid <- NULL
data$ID <- NULL

data$SUMMARY <- as.character(data$SUMMARY)
data$DATA <- as.character(data$DATA)
data$previous_appointment <- tolower(data$previous_appointment)

##########################################################

unique(data$categories)
data$categories <- gsub('as', 'AS', as.character(data$categories), ignore.case = FALSE, perl = FALSE)
data$categories <- gsub('m', 'M', as.character(data$categories), ignore.case = FALSE, perl = FALSE)
data$categories <- gsub('JUNK', NA, as.character(data$categories), ignore.case = FALSE, perl = FALSE)
unique(data$categories)

unique(data$sub_categories)
data$sub_categories <- gsub('m', 'M', as.character(data$sub_categories), ignore.case = FALSE, perl = FALSE)
data$sub_categories <- gsub('JUNK', NA, as.character(data$sub_categories), ignore.case = FALSE, perl = FALSE)
unique(data$sub_categories)

sum(is.na(data))
data <- data[complete.cases(data),]

#data <- arrange(data, categories)
##########################################################

for(i in c(1:53911)){ 
  data$DATA[i]=gsub("\\\\[^\\s]+\\s"," ",data$DATA[i], perl=T) 
  data$DATA[i]=gsub("\\\\par[^}]+}"," ",data$DATA[i], perl=T) 
  data$DATA[i]=gsub("{[^}]+}"," ",data$DATA[i], perl=T) 
  data$DATA[i]=gsub("xxxx-xxxx"," ",data$DATA[i], perl=T)
  data$DATA[i]=gsub("}"," ",data$DATA[i], perl=T)
}

##########################################################

data$SUMMARY <- str_trim(data$SUMMARY)
data_text <- select(data,SUMMARY)
names(data_text) <- "comments"
data_text$comments <- as.character(data_text$comments)

##########################################################

tdm <- function(x, print = TRUE){

  corpus <- Corpus(VectorSource(data$DATA))

  #case folding
  corpus <- tm_map(corpus, tolower)
  
  #Remove stop words
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'), 'phone', 'note', 'arial', 'xxx'))

  #Remove punctuation marks
  corpus <- tm_map(corpus, removePunctuation)
  
  #Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  
  #Remove unnecessary spaces
  corpus <- tm_map(corpus, stripWhitespace)
  
  #Stemming
  
  corpus <- tm_map(corpus, stemDocument)
  
  
  #corpus <- tm_map(corpus, stemCompletion, dictionary = corpus.copy)
  
  #Convert into plain text document
  #corpus <- tm_map(corpus, PlainTextDocument)
  
  #Convert into corpus
  #corpus <- Corpus(VectorSource(corpus))
  
  writeLines(as.character(corpus[[1]]))
  
  tdm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf, min_docfreq = 3))

  tdm2 <- removeSparseTerms(tdm, sparse = 0.996)
  
  df <- as.data.frame(as.matrix(tdm2))
  
  return(tdm)
}  
  
tdm_total <- tdm(data_text$comments)

mat <- as.matrix(tdm_total)
###############################################################

library("e1071")
classifier <- svm(as.factor(categories) ~ ., data = df_final[1:10000,])
predicted <- predict(classifier, df_ask_a_docto[10001:12960,])
table(ask_a_doctor_final$sub_categories[10001:12960], predicted)


###############################################################
#words_freq <- rollup(tdm, 2, na.rm = TRUE, FUN = sum)
#words_freq <- as.matrix(words_freq)
#words_freq <- data.frame(words_freq)
#words_freq$words <- row.names(words_freq)
#row.names(words_freq) <- NULL
#words_freq = words_freq[,c(2,1)]
#names(words_freq) = c("Words", "Frequency")

tdm_dataframe <- as.data.frame(inspect(tdm))
library("tidytext")
tidy_total <- tidy(tdm_total)
#library("topicmodels")
#df_lda <- LDA(tdm, k=2, control = list(seed = 1234))
#df_beta <- tidy(df_lda, matrix = 'beta')
#df_gamma <- tidy(df_lda, matrix = 'gamma')

df1 <- df %>% cast_sparse(document, term, count)
#library("Matrix")
#df2 <- as.data.frame(as.matrix(df1))
df2 <- as.matrix(df1)
