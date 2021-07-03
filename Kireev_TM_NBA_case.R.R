# Date: Jan 20
# Purpose: Clean all my text
# Author: Mikhail Kireev

#Set working directory
setwd("~/Desktop/R/hult_NLP_student/cases/NBA Fan Engagement/data")

#Upload libraries
library(tm)
library(ggplot2)
library(ggthemes)
library(wordcloud)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('SMART'), 'hawks','atlanta', 'a...', 'atlanta', 'atlanta...', 'atlhawks','truetoatlanta')

# Data
textDF      <- read.csv('G_Apr2020.csv',nrows = 1000)

# As of tm version 0.7-3 tabular was deprecated
names(textDF)[1] <-'doc_id' 

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(textDF))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
textTDM  <- TermDocumentMatrix(txtCorpus)
textTDMm <- as.matrix(textTDM)

# Get Row Sums & organize
textTDMv <- sort(rowSums(textTDMm), decreasing = TRUE)
text   <- data.frame(word = names(textTDMv), freq = textTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

#Wordcloud code
set.seed(1234)
wordcloud(text$word,
          text$freq,
          max.words    = 70,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# Frequency Data Frame
textSums <- rowSums(textTDMm)
textFreq <- data.frame(word=names(textSums),frequency=textSums)

# Review a section
textFreq[50:55,]

# Remove the row attributes meta family
rownames(textFreq) <- NULL
textFreq[50:55,]

# Simple barplot; values greater than 15
topWords      <- subset(textFreq, textFreq$frequency >= 56) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

# Inspect word associations with farm
associations <- findAssocs(textTDM, 'farm', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 

# Inspect word associations with statefarm
associations <- findAssocs(textTDM, 'statefarm', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

# Inspect word associations with podcast
associations <- findAssocs(textTDM, 'podcast', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

# Inspect word associations with spotify
associations <- findAssocs(textTDM, 'spotify', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

