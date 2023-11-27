rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs)        # load the library for tk_choose_files
source(file.choose()) # source the function exact.matches.2
options(warn=1)       # make R show any warnings as soon as they arise in the loop

# define the function whitespace
whitespace <- function (input.vector, leadtrail=TRUE, reduce=TRUE, empties=TRUE) {
   if(reduce) { # if reduce is TRUE
      input.vector <- gsub(" {2,}", " ", input.vector, perl=TRUE) # replace all occ of 2+ spaces by 1 space
   }
   if(leadtrail) { # if leadtrail is TRUE
      input.vector <- gsub("(^ +| +$|)", "", input.vector, perl=TRUE) # replace leading/trailing spaces by nothing
   }
   if(!empties) { # if empties is FALSE
      input.vector <- input.vector[nzchar(input.vector)] # do not retain empty character strings in the input vector
   }
   return(input.vector) # return the processed input vector as output
} # end of function definition

# define the directory where the BNC files are located and get the file names
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)
dir.create("../_outputfiles/05_10_freqoutput") # create a subdirectory for the frequency list files



for(i in corpus.files) { # access each file
   cat( # output a progress report to the screen
      basename(i), "\n"
      ) # the file that will be loaded on this iteration, followed by a line break

   current.corpus.file <- tolower(scan(i, what=character(), sep="\n", quiet=TRUE)) # load and tolower the corpus file i

   # retrieve only the lines with sentences in them (i.e., discard the header, utterance tags, etc.)
   current.sentences <- grep("<s n=", # find the lines that have sentence number tags in them
                             current.corpus.file, # in the i-th/current corpus file
                             perl=TRUE,  # using Perl-compatible regular expressions
                             value=TRUE) # and return the (whole line) match, not its position index

   # look for multi-word units and their multi-word-unit tags
   mw.units <- exact.matches.2(   # put into mw.units what you find when you search for
      "<mw c5.*?(?= *</mw>)",     # the beginning of an opening mw tag and before maybe a space and its closing tag
      current.sentences,          # in current.sentences
      gen.conc.output=FALSE)[[1]] # suppress concordance output, retain only exact matches

   if(!is.null(mw.units)) { # conditional expression: if there are multi-word units
      mws.tags <- exact.matches.2("(?<=<mw c5=\")[^\"]+",     # put into mws.tags the c5 annotation from the tag
                                  mw.units,                   # in mw.units
                                  gen.conc.output=FALSE)[[1]] # suppress concordance output, retain only exact matches

      mws.wrds <- strsplit(mw.units,  # put into mws.wrds the words by splitting mw.units up at
                           " *<.*?>", # tags (with maybe preceding spaces)
                           perl=TRUE) # using Perl-compatible regular expressions
      mws.wrds <- whitespace( # put into mws.words the whitespace-cleaned result of
         sapply(mws.wrds,     # applying to mws.wrds
                paste,        # the function paste
                collapse=" ") # merging everything with spaces between things
      )
      mw.units <- paste(mws.tags, # create mw.units by pasting together the mw tags
                        mws.wrds, # with the mw words
                        sep="\t") # separated by tabstops
      mw.units.table <- table(mw.units) # create a frequency table of mw.units

      # delete the multi-word units to avoid counting their components twice
      current.sentences <- gsub("<mw .*?</mw>",    # find tags beginning with "<mw " and what they tag
                                "",                # replace them by nothing
                                current.sentences, # in the character vector current.sentences.1
                                perl=TRUE)         # using Perl-compatible regular expressions
   } # end of the conditional expression

   w.tags <- exact.matches.2("(?<=<w c5=\")[^\"]+",      # retrieve the c5 annotation from the tag
                             current.sentences,          # in current.sentences
                             gen.conc.output=FALSE)[[1]] # suppress concordance output, retain only exact matches

   wrds <- exact.matches.2("[^>]+(?= ?</w>)",          # retrieve characters that are not ">" before maybe a space and then the closing tag
                           current.sentences,          # in current.sentences
                           gen.conc.output=FALSE)[[1]] # suppress concordance output, retain only exact matches

   w.units <- paste(w.tags,           # create w.units by pasting together the w tags
                    whitespace(wrds), # the whitespace-cleaned words
                    sep="\t")         # separated by tabstops
   w.units.table <- table(w.units) # create a frequency table of w.units

   current.file.freqs <- c(mw.units.table, w.units.table) # combine the frequency tables for both
   save(current.file.freqs,  # save that frequency list into
        file=paste0(         # a file ...
           "../_outputfiles/05_10_freqoutput/", # whose path begins with the directory for these lists ...
           substr(basename(i), 1, 3), # whose name is the 3-character identifier of the BNC file name
           ".RData"))        # whose extension is .RData
} # end of loop



setwd("../_outputfiles/05_10_freqoutput") # move to the directory with all the file-specific frequency tables

# define empty vectors into which to insert the words-tag combinations and frequencies of all file-specific frequency tables
all.word.tags <- character(length=25000000)
all.word.tag.freqs <- integer(length=25000000)

# define a vector counter that defines the positions where to put new input into all.word.tags and all.word.tag.freqs
counter <- 1

for(i in dir()) { # access each interim-results RData file
   cat( # output a progress report to the screen
      basename(i), "\n"
      ) # the file that will be loaded on this iteration, followed by a line break
   load(i) # load the lemma frequency list from file i

   all.word.tags[ # put into all.word.tags,
      counter:    # beginning at the current value of counter and
         (counter-1+length(current.file.freqs)) # ending after all new values have been inserted,
   ] <- names(current.file.freqs)               # the counted words of the current interim-results file
   all.word.tag.freqs[ # put into all.word.tag.freqs,
      counter:         # beginning at the current value of counter and
         (counter-1+length(current.file.freqs)) # ending after all new values have been inserted,
   ] <- current.file.freqs                      # the frequencies of the words of the current interim-results file
   counter <- counter+length(current.file.freqs) # set the counter to its new starting position after the inserted material
} # end of loop

all.word.tag.freqs <- as.numeric(all.word.tag.freqs[nzchar(all.word.tags)]) # discard the remaining unused slots
all.word.tags <- all.word.tags[nzchar(all.word.tags)]                       # discard the remaining unused slots

result <- tapply(      # the result of applying
   all.word.tag.freqs, # to all.word.tag.freqs
   all.word.tags,      # a grouping by the names of all.word.tag.freqs
   sum)                # the function sum
   class(result) <- "table" # make sure the data type is a table again
result <- sort(result,          # sort the resulting table
               decreasing=TRUE) # in descending order



# storing the results
# as a csv file (which may not be openable completely in spreadsheet software given its large nrow of 1087409)
cat("TAG\tFORM\tFREQ",   # print a header
    paste(names(result), # followed by the pasted tag-word combinations
          result,        # and their frequencies
          sep="\t"),     # separating columns by tab stops
    sep="\n",            # and rows by linebreaks
    file="../05_10_output.csv") # into this file

temp <- strsplit(names(result), "\t") # split up the tag-word combinations by the tab stops
result.df <- data.frame(       # form a data frame result.df
   TAGS =sapply(temp, "[", 1), # with the tags (first parts of the list elements) in the first column
   FORMS=sapply(temp, "[", 2), # with the words (second parts of the list elements) in the second column
   FREQS=as.numeric(result))   # with the frequencies in the third column
save(result.df, file="../05_10_output.Rdata") # and save it into this file



# explanation/simulation of lines 102-117
# interim results tables like the ones we load
set.seed(1)
(freqs.1 <- table(sample(letters[1:8], 20, replace=TRUE)))  # a 1st one
(freqs.2 <- table(sample(letters[6:12], 20, replace=TRUE))) # a 2nd one

# empty vectors into which to insert ...
(lett.coll <- character(16))
(freq.coll <- numeric(16))

# define a vector counter that ...
counter <- 1

# 'iteration 1'
lett.coll[counter:                       # put into lett.coll starting at position 1
             (counter-1+length(freqs.1)) # ending at position 1 + number of freqs (=8) - 1 = 8
          ] <- names(freqs.1)            # the 8 letters that are the names of the frequencies
lett.coll # see how, now that freqs.1 has been processed, the first 8 slots are now used?

freq.coll[counter:                       # put into freq.coll starting at position 1
             (counter-1+length(freqs.1)) # ending at position 1 + number of freqs (=8) - 1 = 8
          ] <- freqs.1                   # the 8 frequencies
freq.coll # see how, now that freqs.1 has been processed, the first 8 slots are now used?

# that means the first empty slot is now at position 9 and we set counter to that
(counter <- counter+length(freqs.1))

# 'iteration 2'
lett.coll[counter:                       # put into lett.coll starting at position 9
             (counter-1+length(freqs.2)) # ending at position 9 + number of freqs (=7) - 1 = 15
          ] <- names(freqs.2)            # the 7 letters that are the names of the frequencies
lett.coll # see how, now that freqs.2 has been processed, the next 7 slots are now also used?

freq.coll[counter:                       # put into freq.coll starting at position 9
             (counter-1+length(freqs.2)) # ending at position 9 + number of freqs (=7) - 1 = 15
          ] <- freqs.2                   # the 7 frequencies
freq.coll # see how, now that freqs.2 has been processed, the next 7 slots are now used?

# getting rid of the empty elements we didn't fill
(freq.coll <- as.numeric(freq.coll[nzchar(lett.coll)])) # in freq.coll
(lett.coll <- lett.coll[nzchar(lett.coll)])             # in lett.coll

result <- tapply( # the result of applying
   freq.coll,     # to the frequencies
   lett.coll,     # a grouping by the letters
   sum)           # and the function sum
result # see how now all frequencies have been combined: g -> 5, for instance
