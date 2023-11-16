
install.packages("zipfR")
install.packages("qdap")
install.packages("VGAM")
install.packages("rJava")


library(zipfR)
library(VGAM)
library(qdap)

x <- 1:20

freq.dist <- rzipf(n=x, N=10000, s=2)
freq.dist<-sort(freq.dist, decreasing=TRUE)
par(mfrow=c(1,1))
plot(x,freq.dist, ylab="Frequency", type="h",
     main="Zipf distribution, frequency", xlab="Word frequency")


##Dispersion plots

corpus.file <- scan("/Users/Adan Tallman/Desktop/A1E.xml", what="character", sep="\n")
corpus.file.sentences <- grep("<s n=", corpus.file, perl=TRUE, value=TRUE)
library(gsubfn)
tagged.NN0 <- unlist(strapply(corpus.file.sentences,
                              "<w c5=\"NN0(-...)?\" hw=\"\\w+\" pos=\"SUBST\">\\w+ ?</w>",
                             perl=T, backref=1))
head(tagged.NN0)
NN0 <- gsub("<.*?>", "", tagged.NN0, perl=T)
NN0 <- gsub("(^ | +$)", "", NN0, perl=T)
head(NN0)
untagged.words <- unlist(strsplit(corpus.file.sentences, "<.*?>", perl=TRUE))
untagged.words <- gsub("[,;:.?!-]", "", untagged.words)
untagged.words <- gsub("(^ +| +$)", "", untagged.words, perl=TRUE)
untagged.words <- untagged.words[nchar(untagged.words)>0]
untagged.words <- gsub("[,;:.?!-]", "", untagged.words)
untagged.words <- gsub("(^ +| +$)", "", untagged.words, perl=TRUE)
untagged.words <- untagged.words[nchar(untagged.words)>0]
head(untagged.words)
seq.corpus <- seq(1:length(untagged.words))
NN0.positions <- integer()
for (i in 1:length(NN0)) {
  # get the position of the current NN0 in the untagged corpus file
    NN0.position <- which(untagged.words==NN0[i])
  # store the result in NN0.positions and start again
    NN0.positions <- c(NN0.positions, NN0.position)
      }
head(NN0.positions)
NN0.count <- rep(NA, length(seq.corpus))
NN0.count[NN0.positions] <-1
plot(NN0.count, xlab="corpus file", ylab="common nouns", type="h", ylim=c(0,1), yaxt='n')

##Making a dispersion plot

corpus.file <- scan("/Users/Adan Tallman/Desktop/A1E.xml", what="character", sep="\n")
corpus.file.sentences <- grep("<s n=", corpus.file, perl=TRUE, value=TRUE)

tagged.NN0 <- unlist(strapply(corpus.file.sentences,
                              "<w c5=\"NN0(-...)?\" hw=\"\\w+\" pos=\"SUBST\">\\w+ ?</w>",
                              perl=T, backref=1))
head(tagged.NN0)
NN0 <- gsub("<.*?>", "", tagged.NN0, perl=T)
NN0 <- gsub("(^ | +$)", "", NN0, perl=T)
head(NN0)
untagged.words <- unlist(strsplit(corpus.file.sentences, "<.*?>", perl=TRUE))
untagged.words <- gsub("[,;:.?!-]", "", untagged.words)
untagged.words <- gsub("(^ +| +$)", "", untagged.words, perl=TRUE)
untagged.words <- untagged.words[nchar(untagged.words)>0]
untagged.words <- gsub("[,;:.?!-]", "", untagged.words)
untagged.words <- gsub("(^ +| +$)", "", untagged.words, perl=TRUE)
untagged.words <- untagged.words[nchar(untagged.words)>0]
head(untagged.words)
seq.corpus <- seq(1:length(untagged.words))
NN0.positions <- integer()
for (i in 1:length(NN0)) {
  # get the position of the current NN0 in the untagged corpus file
  NN0.position <- which(untagged.words==NN0[i])
  # store the result in NN0.positions and start again
  NN0.positions <- c(NN0.positions, NN0.position)
}
head(NN0.positions)
NN0.count <- rep(NA, length(seq.corpus))
NN0.count[NN0.positions] <-1
plot(NN0.count, xlab="corpus file", ylab="common nouns", type="h", ylim=c(0,1), yaxt='n')


##


##Making a frequency plot

words <- read.csv("/Users/Adan Tallman/Desktop/wordswithq.csv")

library(tidyverse)

glimpse(words)


words <- words[order(match(words$Frequency, words$Item)),]

words_1 <- words[1:20,] 

words_1 <- words_1[order(-words_1$Frequency),]

barplot(words_1$Frequency, names.arg = words_1$Item, horiz=TRUE, las = 1, 
        cex.names = 0.6)

