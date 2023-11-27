rm(list=ls(all=TRUE)) # clear memory

# read in data
textfile <- scan(file.choose(),             # load into textfile the file <_qclwr2/_inputfiles/corp_gpl_long.txt>
                 what=character(),          # as a character vector
                 sep="\n",                  # with linebreaks as separators between vector elements
                 quote="", comment.char="") # and no quote or comment characters
textfile <- tolower(textfile) # convert the file to lower case

# create a vector of all words in the corpus
textfile.words <- strsplit(textfile,  # split up the vector textfile
                           "[^a-z]+", # at 1+ occurrences of characters that are not letters a-z
                           perl=TRUE) # using Perl-compatible regular expressions
textfile.words <- unlist(textfile.words) # change the list into a vector

# remove empty character strings
textfile.words <- textfile.words[nzchar(textfile.words)]



# first way to create word 3-grams the elementary way
# create a character vector with as many empty elements as there will be word 3-grams
# and immediately show how many word 3-grams there will be
length( # show the length of the to-be-created character vector all.3grams1
   all.3grams1 <- # that is
      character(  # an empty character vector that has ...
         length=  # ... as many items as ...
            length(textfile.words) # ... textfile.words has words, minus
         -2) # gramlength-1: if you wanted 3-grams of 10 words the last one would be with words 10-(3-1)=8 to 10
   ) # there will be 2950 word 3-grams

# create all word 3-grams
for(i in seq(all.3grams1)) { # for the i-th word (i iterating from 1 to 2950)
   all.3grams1[i] <-          # store in the i-th element of all.3grams1
      paste(textfile.words[   # the words of textfile.words
         i:            # beginning at i and
         (i+2)],       # ending at i+2
         collapse=" ") # pasted together with a space between them
} # end of loop

# create a sorted frequency table
sorted.ngram.table <- sort(table(all.3grams1), # sort the frequency table of the word 3-grams
                           decreasing=FALSE)   # in ascending order

# compute the deciles of the frequency table
quantile(sorted.ngram.table,   # compute the quantiles of the 3-gram frequencies
         probs=seq(0, 1, 0.1)) # for 10%, 20%, ..., 90%, 100%
# 90% of the word 3-grams occur only 1 time

# visualize the frequencies of the word 3-grams
plot(sorted.ngram.table, type="h", # plot the frequencies of the word 3-grams as vertical lines
     xlab="Word n-grams",          # create an x-axis label
     ylab="Observed frequency")    # create a y-axis label
   abline(h=median(sorted.ngram.table), # add a horizontal line at the median frequency (4)
          lty=2); grid()                # that is dashed and add a grid



# second and nicer way to create word 3-grams from multi-element character vectors
all.3grams2 <- apply( # use / apply to the matrix that
   mapply( # you get from using mapply
      seq, # to generate sequences that
      1:(length(textfile.words)-2), # begin at 1, 2, ..., 2950 and
      3:length(textfile.words)      # end with 3, 4, ..., 2952
   ), # so, use the values from that matrix
   2, # in a columnwise fashion
   function (items) { # an inline/anonymous function that
      paste(textfile.words[items],  # to access and then paste together the subsetted words
            collapse=" ")           # with spaces between them
   } # end of inline/anonymous function
) # end of apply(...)



# ideally, you would now also write your own function that can do this
word.ngrams <- function (input.vector, gram.length) {
   output <- apply( # use the matrix that
      mapply( # you get from using mapply
         seq, # to generate sequences that
         1:(length(input.vector)-(gram.length-1)), # begin at 1, 2, ...
         gram.length:length(input.vector)          # and end with the relevant gramlengths
         ), # use the values from that matrix
      2, # in a columnwise fashion
      function (items) { # an inline/anonymous function that
         paste(input.vector[items], # to access and then paste together the subsetted words
               collapse=" ")        # with spaces between them
      } # end of inline/anonymous function
   ) # end of apply(...)
   return(output) # return the output object just created
} # end of function definition

# this is how you apply that function to a character vector with 2+ element such as textfile.words
all.3grams3 <- word.ngrams(textfile.words, 3)
head(all.3grams3, 50)



# excursus: generating all.3grams2 with the operator %>% from the package dplyr
library(dplyr)
all.3grams2 <-
   mapply(seq,
          1:(length(textfile.words)-2),
          3:length(textfile.words))     %>%
   apply(2, function (items) paste(textfile.words[items], collapse=" "))
