rm(list=ls(all=TRUE)) # clear memory
library(dplyr) # load the library for %>%
library(rChoiceDialogs) # load the library for tk_choose_dir
source(file.choose()) # source the function exact.matches.2

# define the corpus files
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# define directory to collect results
dir.create("../_outputfiles/05_24_freqoutput")

for(i in corpus.files) { # access each corpus file
   cat(basename(i), "\n") # output a progress report to the screen

   current.hyphs.freqs <-                                 # put into current.hyphs.freqs
      scan(i, what=character(), sep="\n", quiet=TRUE) %>% # the result of scanning i into a character vector
      tolower                                         %>% # setting it to lower case
      grep("<s n=", ., value=TRUE)                    %>% # finding the sentences in it
      exact.matches.2(                                    # looking for
         "<w [^>]+>\\w+-[^<-]{2,}(?=</w>)", .,            # tags of hyphenated words
         gen.conc.output=FALSE)                       %>% # with no concordance output
      "[["(1)                                         %>% # but extract only the exact matches
      sub("^.*>", "", ., perl=TRUE)                   %>% # delete the opening tag
      sub(" +$", "", ., perl=TRUE)                    %>% # and any final spaces
      table                                               # and tabulate
   save(current.hyphs.freqs,                             # save that frequency table
        file=paste0("../_outputfiles/05_24_freqoutput/", # into a file in the interim-results directory
                    substr(basename(i), 1, 3),           # with the first three characters of the file name
                    ".RData"))                           # and the extension .RData
} # end of loop



# now we merge all the frequency tables of the individual corpus files into one
setwd("../_outputfiles/05_24_freqoutput") # move to the directory with all the file-specific frequency tables

# define vectors which should be long enough to merge the results
# from all frequency tables from the corpus files
all.hyphs <- character(length=1000000)
all.hyphs.freqs <- numeric(length=1000000)
counter <- 1 # set a position counter for where in the vectors new hyphenated forms get added

for(i in dir()) { # for each file in the interim-results directory
   cat(basename(i), "\n" ) # output progress report
   load(i) # load the frequency list from the i-th/current file

   if(length(current.hyphs.freqs)==0) { # if the current file contains no hyphenated forms
      next                              # go to the next iteration
   }                                    # otherwise
   all.hyphs[counter:(counter-1+length(current.hyphs.freqs))] <- # add to the vector of all hyphenated forms
      names(current.hyphs.freqs)                                 # the ones from the current file
   all.hyphs.freqs[counter:(counter-1+length(current.hyphs.freqs))] <- # add to the vector of all hyphenated forms' frequencies
      current.hyphs.freqs                                              # the ones from the current file
   counter <- counter+length(current.hyphs.freqs) # set the counter to the first position after the last hyphenated form and frequency of hyphenated form respectively
} # end of loop

# get rid of the unused empty strings at the end
all.hyphs.freqs <- all.hyphs.freqs[nzchar(all.hyphs)]
all.hyphs <- all.hyphs[nzchar(all.hyphs)]

result1 <- tapply(   # the result of applying
   all.hyphs.freqs, # to all.word.tag.freqs
   all.hyphs,       # a grouping by the names of all.word.tag.freqs
   sum)             # the function sum
class(result1) <- "table" # make sure the data type is a table again

# sort the resulting table by frequency in descending order and display the top
head(result1 <- sort(result1, decreasing=TRUE))
setwd("../") # switch the working directory back one up



freq.hyphens <- result1[result1>=1000] # restrict the analysis to hyphenated forms of a certain frequency
freq.nothings <- freq.spaces1 <- freq.spaces2 <- # create vectors for forms with spaces and forms without hyphens
   rep(0, length(freq.hyphens))                  # that also have 66 elements each
names(freq.nothings) <- gsub("-", "",             # give the elements of freq.nothings names by deleting the hyphen
                             names(freq.hyphens), # in the vector of hyphenated forms
                             perl=TRUE)           # using Perl-compatible regular expressions
names(freq.spaces1) <- gsub("-", " ",            # give the elements of freq.spaces names by replacing the hyphen w/ spaces
                            names(freq.hyphens), # in the vector of hyphenated forms
                            perl=TRUE)           # using Perl-compatible regular expressions
names(freq.spaces2) <- gsub("-", " ",            # give the elements of freq.spaces names by replacing the hyphen w/ spaces
                            names(freq.hyphens), # in the vector of hyphenated forms
                            perl=TRUE)           # using Perl-compatible regular expressions

search.expressions.nothings <- paste0("<w [^>]+>", names(freq.nothings), " ?</w>")
search.expressions.spaces1 <- paste0("<w [^>]+>", names(freq.spaces1), " ?</w>")
split.up <- strsplit(names(freq.spaces2), " "); parts.1 <- sapply(split.up, "[", 1); parts.2 <- sapply(split.up, "[", 2)
search.expressions.spaces2 <- paste0("<w [^>]+>", parts.1, " ?</w><w [^>]+>", parts.2, " ?</w>")

for(i in corpus.files) { # access each corpus file
   cat(basename(i), "\n") # output a progress report to the screen

   current.sentences <-                                   # put into current.sentences
      scan(i, what=character(), sep="\n", quiet=TRUE) %>% # the result of scanning i as a character vector
      tolower                                         %>% # changing it to lower case
      grep("<s n=", ., value=TRUE)                        # and finding the sentences in it

   freq.nothings <- freq.nothings +               # add to freq.nothings
      lapply(                                     # the result of using
         search.expressions.nothings,             # the forms without spaces or hyphens
         exact.matches.2,                     # as search expressions in exact.matches.2
         corpus.vector=current.sentences,     # to find in current.sentences
         gen.conc.output=FALSE)           %>% # suppress concordances, and pass it on to
      sapply("[", 1)                          %>% # sapply to extract ("[") part 1 of the output, pass it on to
      sapply(length)                              # sapply to measure the length of the exact matches output

   freq.spaces1 <- freq.spaces1 +         # add to freq.spaces
      lapply(                             # the result of using
         search.expressions.spaces1,      # the forms with spaces
         exact.matches.2,                 # as search expressions in exact.matches.2
         corpus.vector=current.sentences, # to find in current.sentences
         gen.conc.output=FALSE)       %>% # suppress concordances, and pass it on to
      sapply("[", 1)                      %>% # sapply to extract ("[") part 1 of the output, pass it on to
      sapply(length)                          # sapply to measure the length of the exact matches output

   freq.spaces2 <- freq.spaces2 +         # add to freq.spaces
      lapply(                             # the result of using
         search.expressions.spaces2,      # the forms with spaces
         exact.matches.2,                 # as search expressions in exact.matches.2
         corpus.vector=current.sentences, # to find in current.sentences
         gen.conc.output=FALSE)       %>% # suppress concordances, and pass it on to
      sapply("[", 1)                      %>% # sapply to extract ("[") part 1 of the output, pass it on to
      sapply(length)                          # sapply to measure the length of the exact matches output

} # end of loop

# compile the result in a single data frame
result2 <- data.frame(HYPHENATED=as.numeric(freq.hyphens),
                      SPACED1=as.numeric(freq.spaces1),
                      SPACED2=as.numeric(freq.spaces2),
                      NOTHING=as.numeric(freq.nothings),
                      row.names=names(freq.hyphens))
head(result2)
save(result2, file="05_24b_output.RData")

# generate a table of percentages that lists for each expression how often it is attested in which spelling
result.perc <- round(prop.table(as.matrix(result2), 1), 3)
result.perc[order(-result.perc[,1]),]
