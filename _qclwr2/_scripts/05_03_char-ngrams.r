rm(list=ls(all=TRUE)) # clear memory

# read in data
textfile <- scan(file.choose(),    # load into textfile the file <_qclwr2/_inputfiles/corp_gpl_long.txt>
                 what=character(), # as a character vector
                 sep="\n",         # with linebreaks as separators between vector elements
                 quote="")         # and no quote or comment characters
textfile <- tolower(textfile) # convert the file to lower case

# delete characters to be discarded
textfile.cleaned <- gsub("[^a-z ]+", # find all characters from the class 'not a letter from a-z or a space'
                         " ",        # replace them by nothing
                         textfile,   # in the vector textfile
                         perl=TRUE)  # using Perl-compatible regular expressions

# merge into one element
textfile.oneline.1 <-      # create a new 1-element character vector
   paste(textfile.cleaned, # by pasting together the elements of textfile.cleaned
         collapse=" ")     # with spaces between them

# delete excess whitespace
textfile.oneline.2 <- gsub("\\s{2,}",          # find all occurrences of 2+ spaces
                           " ",                # replace them by nothing
                           textfile.oneline.1, # in the vector textfile.oneline.1
                           perl=TRUE)          # using Perl-compatible regular expressions

# for better legibility of the final output (of the n-grams), change " " to "_"
textfile.oneline.3 <- gsub(" ",                # find all occurrences of a spaces
                           "_",                # replace them by "_"
                           textfile.oneline.2, # in the vector textfile.oneline.2
                           perl=TRUE)          # using Perl-compatible regular expressions



# first way to create character 3-grams the elementary way
# create a character vector with as many empty elements as there will be character 3-grams
# and immediately show how many character 3-grams there will be
length( # show the length of the to-be-created character vector all.3grams1
   all.3grams1 <- # that is
      character(  # an empty character vector that has ...
         length=  # ... as many items as ...
            nchar(textfile.oneline.3) # ... textfile.oneline.3 has characters, minus
         -2) # gramlength-1: if you wanted 3-grams of 10 characters the last one would be from 10-(3-1)=8 to 10
   ) # end of length(...): there will be 17094 character 3-grams

# create all character 3-grams
for(i in seq(all.3grams1)) {    # for the i-th character (i iterating from 1 to 17094)
   all.3grams1[i] <-             # store in the i-th element of all.3grams1
      substr(textfile.oneline.3, # the substring of textfile.oneline.3
             i,                  # beginning at i and
             i+2)                # ending at i+2
} # end of loop

# create a sorted frequency table
sorted.ngram.table <- sort(table(all.3grams1), # sort the frequency table of the character 3-grams
                           decreasing=FALSE)   # in ascending order

# compute the deciles of the frequency table
quantile(sorted.ngram.table,   # compute the quantiles of the 3-gram frequencies
         probs=seq(0, 1, 0.1)) # for 10%, 20%, ..., 90%, 100%
# 90% of the character 3-grams occur only up to 24 times

# visualize the frequencies of the character 3-grams
plot(sorted.ngram.table, type="h", # plot the frequencies of the character 3-grams as vertical lines
     xlab="Character n-grams",     # create an x-axis label
     ylab="Observed frequency")    # create a y-axis label
   abline(h=median(sorted.ngram.table), # add a horizontal line at the median frequency (4)
          lty=2); grid()                # that is dashed; add a grid



# second and nicer way to create character 3-grams from 1-element character vectors
all.3grams2 <- mapply(substr,                          # apply the function substr
                      textfile.oneline.3,              # to a 1-element character vector
                      1:(nchar(textfile.oneline.3)-2), # where the substrings start at 1, 2, ..., 17094 and
                      3:nchar(textfile.oneline.3),     # where the substrings end at   3, 4, ..., 17096
                      USE.NAMES=FALSE)                 # don't return names of the character n-grams



# third and nicer way to create character 3-grams from 1-element character vectors
all.3grams3 <- substr(                                     # apply substr to
   rep(textfile.oneline.3, (nchar(textfile.oneline.3)-2)), # as many repetitions of the input vector as there are 3-grams
   1:(nchar(textfile.oneline.3)-2),                        # where the substrings start at 1, 2, ..., 17094 and
   3:nchar(textfile.oneline.3)                             # where the substrings end at 3, 4, ..., 17096
) # end of substr(...)



# ideally, you would now also write your own function that can do this
character.ngrams <- function (input.vector, gram.length) {
   output <- mapply(substr,          # apply the function substr
                    input.vector,    # to the 1-element character vector used as input
                    1:(nchar(input.vector)-(gram.length-1)), # where the substrings start at 1, 2, ...
                    gram.length:nchar(input.vector),         # and end with the relevant gramlengths
                    USE.NAMES=FALSE) # don't return names of the character n-grams
   return(output) # return the output object just created
} # end of function definition

# this is how you apply that function to a character vector with 1 element such as textfile.oneline.3
all.3grams4 <- character.ngrams(textfile.oneline.3, 3)
head(all.3grams4, 50)

# this is how you apply that function to a character vector with 2+ elements such as textfile.cleaned
all.3grams5 <-              # create an object all.3grams4 and show its head
   lapply(textfile.cleaned, # apply to each element of a multi-element character vector
          character.ngrams, # the function character/ngrams
          3)                # with the gramlength of 3
head(all.3grams5)         # check out the first 6 elements of that list
head(unlist(all.3grams5), 50) # check out the first 50 elements of the unlisted character vector
