rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.files
library(XML)   # load the package XML for XML processing
library(dplyr) # load the package dplyr for %>%

# define the function whitespace
whitespace <- function (input.vector, leadtrail=TRUE, reduce=TRUE, empties=TRUE) {
   if(reduce) { # if reduce is TRUE
      input.vector <- gsub(" {2,}", " ", input.vector, perl=TRUE) # replace all occ of 2+ spaces by 1 space
   }
   if(leadtrail) { # if leadtrail is TRUE
      input.vector <- gsub("(^ +| +$|)", "", input.vector, perl=TRUE) # replace leading/trailing spaces by nothing
   }
   if(!empties) { # if empties is FALSE
      input.vector <- input.vector[nzchar(input.vector)]
   }
   return(input.vector)
}

corpus.file <- xmlInternalTreeParse(rchoose.files())
corpus.words <- xpathSApply(corpus.file, "//w", xmlValue)
summary(nchar(corpus.words))
 # median=5, mean=5.665. That means, to arrive at a roughly 80-character line, we take about 12 words (because we need to reserve some space for the line-initial annotation and we must not forget that the above nchar includes the spaces after words.

line.assigner <- as.numeric(         # make line.assigner a numeric vector version of
   cut(                              # the factor you get when you cut
      seq(corpus.words),             # the sequence of words up into
      breaks=length(corpus.words)/12 # 12-word elements
      ) # end of cut(..)
)
table(table(line.assigner))
# as planned, nearly all lines will have 12 words (of course those will sometimes amount to more than 80 characters, but, c'mon, gimme a break ;-) ...)

corpus.lines <-      # put into corpus.lines the result of
   tapply(           # applying
      corpus.words,  # to the words in the corpus
      line.assigner, # a grouping by their assigned line numbers
      paste,         # the function paste
      collapse=" ")  # with collapse=" "

corpus.lines <- whitespace(paste( # remove all whitespace annoyances from pasting
   paste("HHV",                         # the result of pasting "HHV" followed by
         formatC(unique(line.assigner), # the unique values of line.assigner
                 width=5,               # formatted to be 5 characters wide
                 digits=5,              # with 5 digits (to avoid R representing 10000 as 1e+04)
                 flag=0),               # and padding of numbers with left 0s,
         corpus.lines)            # and the corpus lines
)) # end of whitespace(paste(...))
# this making-the-line-numbers-5-digits-wide is not just me being anal: it guarantees that the format and string width of the numbering etc. is perfectly consistent: you know for certain that the actual corpus text starts at character slot 11, which helps, for instance, if you ever needed to apply substr to these data.

head(corpus.lines, 25) # looks good
tail(corpus.lines, 25) # looks good

cat(corpus.lines, file="../_outputfiles/05_22c_BNC-HHVasBROWN.txt", sep="\n")



# if you really want to knock yourself out, try to replicate the Brown corpus format much more precisely than I did here by
# - making sure every sentence is preceded by 3 spaces (that one is simple) and ...
# - making sure no line is more than 80 characters long but that no vector element / linebreak interrupts a word ...
# Enjoy ...
