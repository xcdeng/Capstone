library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(stringi)
library(slam)
library(RWeka)
library(ggplot2)

setwd("D:/R/capstone/final/en_US")
twitter <- readLines(con <- file("./twitter.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)
news <- readLines(con <- file("./news.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)
blogs <- readLines(con <- file("./blogs.txt"), encoding = "UTF-8", skipNul = TRUE)
close(con)


twitter<-sample(twitter, 20000)
news<-sample(news, 20000)
blogs<-sample(blogs, 20000)
dataall<-c(twitter,news,blogs)

dataall1<- iconv(dataall, 'UTF-8', 'ASCII', "byte")
dataVec <- VectorSource(dataall1)  
dataCorpus <- Corpus(dataVec)
dataCorpus<- tm_map(dataCorpus, tolower)
dataCorpus<- tm_map(dataCorpus, removeNumbers)
dataCorpus<- tm_map(dataCorpus, removePunctuation)
dataCorpus<- tm_map(dataCorpus, stripWhitespace)

datasample <- tm_map(dataCorpus, PlainTextDocument)

save(datasample,file="sampledata.RData")
load("sampledata.RData") 


finaldata<-readRDS("./sampledata.rds")


unigram <- NGramTokenizer(dataCorpus, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
unigram <- data.frame(table(unigram))
unigram <- unigram[order(unigram$Freq,decreasing = TRUE),]
names(unigram) <- c("word1", "freq")
write.csv(unigram[unigram$freq > 1,],"unigram.csv",row.names=F)
unigram <- read.csv("unigram.csv",stringsAsFactors = F)
saveRDS(unigram, file = "unigram.RData")
unigram <- readRDS("unigram.RData")
g1 <- ggplot(data=unigram[1:10,], aes(x = word1, y = freq))
g2 <- g1 + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequently Words")
g3 <- g2 + geom_text(data = unigram[1:10,], aes(x = word1, y = freq, label = freq), hjust=-1, position = "identity")
g3


bigram <- NGramTokenizer(dataCorpus, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
bigram <- data.frame(table(bigram))
bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]
names(bigram) <- c("words","freq")
bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram,
                    one = sapply(str2,"[[",1),
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)
write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")


trigram <- NGramTokenizer(dataCorpus, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
trigram <- data.frame(table(trigram))
trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]
head(trigram)
names(trigram) <- c("words","freq")
head(trigram)

trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
trigram <- data.frame(word1 = trigram$one,word2 = trigram$two,
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)
write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")


quadgram <- NGramTokenizer(dataCorpus, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
quadgram <- data.frame(table(quadgram))
names(quadgram) <- c("words","freq")
quadgram$words <- as.character(quadgram$words)
str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3),
                      four=sapply(str4,"[[",4))
quadgram <- data.frame(word1 = quadgram$one,word2 = quadgram$two,
                       word3 = quadgram$three, word4 = quadgram$four,freq = quadgram$freq,stringsAsFactors=FALSE)
write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")