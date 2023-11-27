rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the package for tk_choose_files
library(XML)   # load the package XML for XML processing
library(xml2)  # load the package xml2 for XML processing: 423, 6708,
source(file.choose()) # source the function exact.matches.2

# define the corpus files
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# define vectors to collect results
nos.of.verbs <- nos.of.words <- nos.of.sentences <- integer(length=length(corpus.files))

for(i in seq(corpus.files)) { # access each corpus file
   cat( # output a progress report to the screen
      i/length(corpus.files), "\n"
      ) # the proportion of files dealt with after this iteration

   if(.Platform$OS.type=="windows") { # if you are using this on a Windoze system
      # use the package xml2
      current.corpus.file <- read_xml(corpus.files[i]) # import a corpus file and parse its XML tree

      # store in the i-th slot of the respective collector vector
      nos.of.sentences[i] <- length(xml_find_all(current.corpus.file, ".//s")) # the # of sentences of the i-th file
      nos.of.words[i] <- length(words <- xml_find_all(current.corpus.file, ".//w")) # the # of words of the i-th file
      nos.of.verbs[i] <- length(        # the number of matches when
         exact.matches.2(               # you look for
            "pos=\"VERB\"",             # the pos-tag VERB
            as.character(words),        # in the character-string version of words
            gen.conc.output=FALSE)[[1]] # suppress the concordance output and retain just the exact matches
      ) # i.e., the # of verbs of the i-th file
   } else {                           # if you are using this on another operating system
      # use the package XML
      current.corpus.file <- xmlInternalTreeParse(corpus.files[i])

      # store in the i-th slot of the respective collector vector
      nos.of.sentences[i] <- summary(current.corpus.file)[[1]]["s"]      # number of sentences of the i-th file
      nos.of.words[i] <- summary(current.corpus.file)[[1]]["w"]          # number of words of the i-th file
      nos.of.verbs[i] <- sum(unlist(            # sum up the logical vector checking
         xpathApply(current.corpus.file, "//w", # whether in the word units
                    xmlGetAttr, "pos")          # the "pos attribute
         )=="VERB")                             # has the value "VERB"   # number of verbs of the i-th file
   } # end of conditional expression testing for the operating system
} # end of loop

FichtnersC <- (nos.of.verbs/nos.of.sentences) *
   (nos.of.words/nos.of.sentences)             # compute Fichtner's C for all files
   names(FichtnersC) <- basename(corpus.files) # name the C-values with the respective file names

# output the results in a nice tabular format
result <- data.frame(          # create a data frame containing
   SENTENCES=nos.of.sentences, # a column with the number of sentences
   VERBS=nos.of.verbs,         # a column with the number of verbs
   WORDS=nos.of.words,         # a column with the number of words
   VERBSperSENT=round(nos.of.verbs/nos.of.sentences, 1), #
   WORDSperSENT=round(nos.of.words/nos.of.sentences, 1),
   FichtnersC
)
summary(result) # look at the summary of this data frame to get an idea of the distributions of the variables
# Notice something rare in the distribution of the Fichtner's C values? The mean is higher than the 3rd quartile!
save(result, file="../_outputfiles/05_07b_resultofloop.RData")



# visual representation
par(mfrow=c(2, 5)) # set up a plotting window with 10 panels (2 rows * 5 columns)
apply(result[,1:5],                      # apply to the data in the first 5 columns of result
      2,                                 # column-wise
      function (input.column) {          # an inline/anonymous function
         hist(input.column, breaks="FD") # that plots a histogram with a particular way to compute bin widths
      }                                  # end of inline/anonymous function
) # end of apply(...)
apply(result[,1:5],                           # apply to the data in the first 5 columns of result
      2,                                      # column-wise
      function (input.column) {               # an inline/anonymous function
         hist(log(input.column), breaks="FD") # that plots a histogram of the logs of the columns with, again, breaks="FD"
      }                                       # end of inline/anonymous function
) # end of apply(...)
par(mfrow=c(1, 1)) # restore the default 1-row, 1-column plotting window

# graphical representation of the more reasonable logs
# we will set up this plot arrangement, all plots are of logs (even if not mentioned below)
layout.show(temp <- layout(matrix(c(1, 1, 2, 2, 3, 3, 0, 4, 4, 5, 5, 0), byrow=TRUE, nrow=2)))

layout(matrix(c(1, 1, 2, 2, 3, 3, 0, 4, 4, 5, 5, 0), byrow=TRUE, nrow=2))
   plot(log(FichtnersC) ~ log(nos.of.verbs),             # plot Fichtner's Cs as a function of the numbers of verbs
        pch=16, col=rgb(0, 0, 0, 20, maxColorValue=255), # with light grey filled circles with alpha transparency
        xlim=c(0, 13), ylim=c(0, 13),                    # set x- and y-axis limits (here to min=0 and max=13 for both)
        xlab="Number of verbs (logged)", ylab="Fichtner's C (logged)",     # label the axes
        main="Fichtner's C ~ # of verbs (both logged)"); grid()            # label the plot and add a grid
      abline(h=median(log(FichtnersC)), lty=2); abline(v=median(log(nos.of.verbs)), lty=2) # add dashed lines for medians
      lines(lowess(log(FichtnersC) ~ log(nos.of.verbs)), lwd=3, col="red") # add a thick (lwd=3) red smoother
   plot(log(FichtnersC) ~ log(nos.of.words),             # plot Fichtner's Cs as a function of the numbers of words
        pch=16, col=rgb(0, 0, 0, 20, maxColorValue=255), # with light grey filled circles with alpha transparency
        xlim=c(0, 13), ylim=c(0, 13),                    # set x- and y-axis limits (here to min=0 and max=13 for both)
        xlab="Number of words (logged)", ylab="Fichtner's C (logged)", # label the axes
        main="Fichtner's C ~ # of words (both logged)"); grid()        # label the plot and add a grid
      abline(h=median(log(FichtnersC)), lty=2); abline(v=median(log(nos.of.words)), lty=2) # add dashed lines for medians
      lines(lowess(log(FichtnersC) ~ log(nos.of.words)), lwd=3, col="red")
   plot(log(FichtnersC) ~ log(nos.of.sentences),         # plot Fichtner's Cs as a function of the numbers of sentences
        pch=16, col=rgb(0, 0, 0, 20, maxColorValue=255), # with light grey filled circles with alpha transparency
        xlim=c(0, 13), ylim=c(0, 13),                    # set x- and y-axis limits (here to min=0 and max=13 for both)
        xlab="Number of sentences (logged)", ylab="Fichtner's C (logged)",     # label the axes
        main="Fichtner's C ~ # of sentences (both logged)"); grid()            # label the plot and add a grid
      abline(h=median(log(FichtnersC)), lty=2); abline(v=median(log(nos.of.sentences)), lty=2) # add dashed lines for medians
      lines(lowess(log(FichtnersC) ~ log(nos.of.sentences)), lwd=3, col="red") # add a thick (lwd=3) red smoother
   plot(log(FichtnersC) ~ log(result$VERBSperSENT),      # plot Fichtner's Cs as a function of the numbers of verbs/sentences
        pch=16, col=rgb(0, 0, 0, 20, maxColorValue=255), # with light grey filled circles with alpha transparency
        xlim=c(-2, 5), ylim=c(0, 13),                    # set x- and y-axis limits
        xlab="Verbs/sentence (logged)", ylab="Fichtner's C (logged)",             # label the axes
        main="Fichtner's C ~ # of verbs/sentence (both logged)"); grid()          # label the plot and add a grid
      abline(h=median(log(FichtnersC)), lty=2); abline(v=median(log(result$VERBSperSENT)), lty=2) # add dashed lines for medians
      lines(lowess(log(FichtnersC) ~ log(result$VERBSperSENT)), lwd=3, col="red") # add a thick (lwd=3) red smoother
   plot(log(FichtnersC) ~ log(result$WORDSperSENT),      # plot Fichtner's Cs as a function of the numbers of words/sentence
        pch=16, col=rgb(0, 0, 0, 20, maxColorValue=255), # with light grey filled circles with alpha transparency
        xlim=c(1, 7), ylim=c(0, 13),                     # set x- and y-axis limits
        xlab="Words/sentence (logged)", ylab="Fichtner's C (logged)",             # label the axes
        main="Fichtner's C ~ # of words/sentence (both logged)"); grid()          # label the plot and add a grid
      abline(h=median(log(FichtnersC)), lty=2); abline(v=median(log(result$WORDSperSENT)), lty=2) # add dashed lines for medians
      lines(lowess(log(FichtnersC) ~ log(result$WORDSperSENT)), lwd=3, col="red") # add a thick (lwd=3) red smoother
par(mfrow=c(1, 1)) # restore the default 1-row, 1-column plotting window

cor.test(log(FichtnersC), log(result$WORDSperSENT)) # extremely high correlation!
cor.test(log(FichtnersC), log(result$VERBSperSENT)) # extremely high correlation!
