# Title: Sentiment Analysis
# Purpose: Analyze themes related to existing teams social media success
# Author: Mikhail Kireev
# email: mkireev2019@student.hult.edu
# Date: Jan 26 2021

setwd("~/Desktop/R/hult_NLP_student/cases/Call of Duty E-Sport/teamFollowerTimelines")

#Libraries
library(tm)
library(qdap)
library(lexicon)
library(ggplot2)
library(plyr)
library(dplyr)
library(fst)
library(pbapply)
library(mgsub)
library(tidytext)
library(reshape2)
library(wordcloud)
library(viridisLite)
library(radarchart)



#This code is a file of where the options and functions are at
source('~/Desktop/R/hult_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')


#------------------------THIS PART IS EXPLORING ATLFAZE------------------------#


# This is where we import dataset
ATLFaZeDF <- read_fst('student_2020-12-28_ATLFaZe2_followers_timelines.fst')

#get only 1k rows
ATLFaZeDF <- ATLFaZeDF[sample(1:nrow(ATLFaZeDF), 1000,
                          replace=FALSE),]

#check if its actually 1k rows
dim(ATLFaZeDF)



#This is where we import the emoji 
emoji <- read.csv('~/Desktop/R/hult_NLP_student/cases/Call of Duty E-Sport/emojis.csv')

#This is where we match the emoji to the tweets/text column
ATLFaZeDF$text <- pbsapply(as.character(ATLFaZeDF$text), mgsub, emoji$emoji, emoji$name)


#Again choose our stop words
stops <- c(stopwords('SMART'),'dell','laptop', 'inspiron', 
           'issue', 'support','problem', 'dellname','community') 


#  VCorpus
ATLFaZetxt <- VCorpus(VectorSource(ATLFaZeDF$text))
ATLFaZetxt <- cleanCorpus(ATLFaZetxt, stops)
content(ATLFaZetxt[[1]])


# This is where we extract the needed column for our analysis
Tweets <- data.frame(document = seq_along(ATLFaZeDF$text), #simple id order
                          status_id = ATLFaZeDF$status_id, # keep track of posts
                          screen_name = ATLFaZeDF$screen_name,
                          text = unlist(sapply(ATLFaZetxt, `[`, "content")),stringsAsFactors=F)

# This is where we start doing the the polarity function 
polarity <- polarity(Tweets$text,Tweets$screen_name)
polarity$group

#This is where we match the polarity score with every tweet value
Tweets$polarityValue <- polarity$all$polarity
Tweets$polarityValue[is.na(Tweets$polarityValue)] <- 0

# this is where we score the polarity (positive/negative)
Tweets$polarityClass <- ifelse(Tweets$polarityValue>0, 'positive',
                                    ifelse(Tweets$polarityValue<0, 'negative', 'neutral'))


#Here we create another document term matrix and assign the polarity to every word
tweetsDTM   <- DocumentTermMatrix(VCorpus(VectorSource(Tweets$text)))
tidyCorp <- tidy(tweetsDTM)
tidyCorp


# Get nrc lexicon; prep of radar chart
nrc <- nrc_emotions
head(nrc)

#cleaning up the lexicon above
terms <- subset(nrc, rowSums(nrc[,2:9])!=0)
sent  <- apply(terms[,2:ncol(terms)], 1, function(x)which(x>0))
head(sent)

# Reshape: 
nrcLex <- list()
for(i in 1:length(sent)){
  x <- sent[[i]]
  x <- data.frame(term      = terms[i,1],
                  sentiment = names(sent[[i]]))
  nrcLex[[i]] <- x
}
nrcLex <- do.call(rbind, nrcLex)
head(nrcLex)

# Perform Inner Join: inner join means combining two tables! (From fan tweet and the emoji sentiment)
nrcSent <- inner_join(tidyCorp,nrcLex, by=c('term' = 'term'))
nrcSent

# Radar Chart (struggle interpreting this radar chart since it's lots of data compared to prof's example)
emos <- aggregate(count ~ sentiment + document, nrcSent, sum)
emos$document <- NULL
chartJSRadar(scores = emos, labelSize = 10, showLegend = F)


#textdata::lexicon_nrc() we do this again for the cloud
nrc     <- get_sentiments(lexicon = c("nrc"))
nrcSent <- inner_join(tidyCorp,nrc, by=c('term' = 'word'))
nrcSent

# Now group by document and select the most numerous 
grpSent <- nrcSent %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpSent$document <- as.numeric(as.character(grpSent$document))
grpSent

# Cast to wide format
wideSent <- dcast(grpSent, document~sentiment,fun.aggregate = sum,value.var = "n")
head(wideSent) 
wideSent[grep('\\b100\\b',wideSent$document),] 

# Drop positive/negative & get maximum column
wideSent <- wideSent[,-c(7,8)]
wideSent$maxEmotion <- ifelse(rowSums(wideSent[,2:ncol(wideSent)])>0,
                              names(wideSent)[2:ncol(wideSent)][max.col(wideSent[,2:ncol(wideSent)])],
                              'noEmotion')
head(wideSent)

# This part is for neutral 
Tweets <- left_join(Tweets, wideSent, by = c('document'='document'))
Tweets$maxEmotion[is.na(Tweets$maxEmotion)] <- 'noEmotion' 
head(Tweets)

# Begin making comparison cloud
polarityLst <- list()
for(i in 1:length(unique(Tweets$polarityClass))){
  x <- subset(Tweets$text, Tweets$polarityClass == unique(Tweets$polarityClass)[i])
  x <- paste(x, collapse = ' ')
  polarityLst[[unique(Tweets$polarityClass)[i]]] <- x
}

# Using the list
allPolarityClasses <- do.call(rbind, polarityLst)
allPolarityClasses <- VCorpus(VectorSource(allPolarityClasses))
allPolarityClasses <- TermDocumentMatrix(cleanCorpus(allPolarityClasses, stops))
allPolarityClasses <- as.matrix(allPolarityClasses)

# Add the names from the list, get the order right!
colnames(allPolarityClasses) <- names(polarityLst)

# Comparison Cloud
comparison.cloud(allPolarityClasses, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(allPolarityClasses),"Dark2"),
                 scale=c(3,0.1))

#------------------------THIS PART IS EXPLORING DALLASEMPIRE-----------------------#


# This is where we import  dataset
DallEmp <- read_fst('student_2020-12-28_dallasempire2_followers_timelines.fst')

#get only 1k rows
DallEmpDF <- ATLFaZeDF[sample(1:nrow(DallEmpDF), 1000,
                              replace=FALSE),]

#check if its actually 1k rows
dim(DallEmpDF)



#This is where we import the emoji 
emoji <- read.csv('~/Desktop/R/hult_NLP_student/cases/Call of Duty E-Sport/emojis.csv')

#This is where we match the emoji to the tweets/text column
DallEmpDF$text <- pbsapply(as.character(ATLFaZeDF$text), mgsub, emoji$emoji, emoji$name)


#Again choose our stop words
stops <- c(stopwords('SMART'),'dell','laptop', 'inspiron', 
           'issue', 'support','problem', 'dellname','community') 


# VCorpus
DallEmptxt <- VCorpus(VectorSource(DallEmpDF$text))
DallEmptxt <- cleanCorpus(DallEmpetxt, stops)
content(DallEmptxt[[1]])


# This is where we extract the needed column for our analysis
Tweets <- data.frame(document = seq_along(DallEmpDF$text), #simple id order
                     status_id = DallEmpDF$status_id, # keep track of posts
                     screen_name = DallEmpDF$screen_name,
                     text = unlist(sapply(DallEmptxt, `[`, "content")),stringsAsFactors=F)

# This is where we start doing the the polarity function 
polarity <- polarity(Tweets$text,Tweets$screen_name)
polarity$group

#This is where we match the polarity score with every tweet value
Tweets$polarityValue <- polarity$all$polarity
Tweets$polarityValue[is.na(Tweets$polarityValue)] <- 0

# this is where we score the polarity (positive/negative)
Tweets$polarityClass <- ifelse(Tweets$polarityValue>0, 'positive',
                               ifelse(Tweets$polarityValue<0, 'negative', 'neutral'))


#Here we create another document term matrix and assign the polarity to every word
tweetsDTM   <- DocumentTermMatrix(VCorpus(VectorSource(Tweets$text)))
tidyCorp <- tidy(tweetsDTM)
tidyCorp


# Get nrc lexicon; prep of radar chart
nrc <- nrc_emotions
head(nrc)

#cleaning up the lexicon above
terms <- subset(nrc, rowSums(nrc[,2:9])!=0)
sent  <- apply(terms[,2:ncol(terms)], 1, function(x)which(x>0))
head(sent)

# Reshape: 
nrcLex <- list()
for(i in 1:length(sent)){
  x <- sent[[i]]
  x <- data.frame(term      = terms[i,1],
                  sentiment = names(sent[[i]]))
  nrcLex[[i]] <- x
}
nrcLex <- do.call(rbind, nrcLex)
head(nrcLex)

# Perform Inner Join: inner join means combining two tables! (From fan tweet and the emoji sentiment)
nrcSent <- inner_join(tidyCorp,nrcLex, by=c('term' = 'term'))
nrcSent

# Radar Chart (struggle interpreting this radar chart since it's lots of data compared to prof's example)
emos <- aggregate(count ~ sentiment + document, nrcSent, sum)
emos$document <- NULL
chartJSRadar(scores = emos, labelSize = 10, showLegend = F)


#textdata::lexicon_nrc() we do this again for the cloud
nrc     <- get_sentiments(lexicon = c("nrc"))
nrcSent <- inner_join(tidyCorp,nrc, by=c('term' = 'word'))
nrcSent

# Now group by document and select the most numerous 
grpSent <- nrcSent %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpSent$document <- as.numeric(as.character(grpSent$document))
grpSent

# Cast to wide format
wideSent <- dcast(grpSent, document~sentiment,fun.aggregate = sum,value.var = "n")
head(wideSent) 
wideSent[grep('\\b100\\b',wideSent$document),] 

# Drop positive/negative & get maximum column
wideSent <- wideSent[,-c(7,8)]
wideSent$maxEmotion <- ifelse(rowSums(wideSent[,2:ncol(wideSent)])>0,
                              names(wideSent)[2:ncol(wideSent)][max.col(wideSent[,2:ncol(wideSent)])],
                              'noEmotion')
head(wideSent)

# This part is for neutral 
Tweets <- left_join(Tweets, wideSent, by = c('document'='document'))
Tweets$maxEmotion[is.na(Tweets$maxEmotion)] <- 'noEmotion' 
head(Tweets)

# Begin making comparison cloud
polarityLst <- list()
for(i in 1:length(unique(Tweets$polarityClass))){
  x <- subset(Tweets$text, Tweets$polarityClass == unique(Tweets$polarityClass)[i])
  x <- paste(x, collapse = ' ')
  polarityLst[[unique(Tweets$polarityClass)[i]]] <- x
}

# Using the list
allPolarityClasses <- do.call(rbind, polarityLst)
allPolarityClasses <- VCorpus(VectorSource(allPolarityClasses))
allPolarityClasses <- TermDocumentMatrix(cleanCorpus(allPolarityClasses, stops))
allPolarityClasses <- as.matrix(allPolarityClasses)

# Add the names from the list, get the order right!
colnames(allPolarityClasses) <- names(polarityLst)

# Comparison Cloud
comparison.cloud(allPolarityClasses, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=1,
                 colors=brewer.pal(ncol(allPolarityClasses),"Dark2"),
                 scale=c(3,0.1))
