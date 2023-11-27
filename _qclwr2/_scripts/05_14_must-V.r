rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for tk_choose_files
source(file.choose()) # source the function exact.matches.2

# define the function just.matches
just.matches <- function(search.expression, corpus, pcre=TRUE, vectorize=TRUE, ...) {
   output <- regmatches(corpus,           # generate an object out by retrieving from the corpus
              gregexpr(search.expression, # the matches when you use gregexpr to find the search expression
                       corpus,            # in the corpus
                       perl=pcre),        # using Perl-compatible regular expressions
                       ...)               # and apply these other arguments to regmatches
   if(vectorize) {             # if the output is supposed to be a vector
      output <- unlist(output) # make output the unlisted version of output
   }                           # end of conditional expression
   return(output) # return the function's output to the user
} # end of function

# define the directory where the BNC XML files, folder K, are located and get the file names
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# prepare collectors and results vectors
infs.after.must <- character() # for all lemmas/infinitives after must as a modal
modals.plus.inf <- 0           # the counter for all modal verbs (i.e. a+b+c+d of all tables like Table 15)

for(i in corpus.files) {
   cat(basename(i), "\n") # output a 'progress report'

   current.corpus.file <- scan(i, what=character(), sep="\n", quiet=TRUE) # load the i-th/current corpus file
   current.sentences <- tolower(grep("<s n=", # find and change to lower case the lines with sentence number tags
                                     current.corpus.file, # in the i-th/current corpus file
                                     perl=TRUE,   # using Perl-compatible regular expressions
                                     value=TRUE)) # and return the (whole line) match, not its position index

   current.mpis <- just.matches(                     # matches when you look for
         "<w c5=\"vm0\"[^<]+</w><w c5=\"vvi\"[^<]+", # any modal verbs followed by infinitives
         current.sentences                           # in current.sentences
      ) # end of just.matches(...)
   modals.plus.inf <- modals.plus.inf + # add to the previous value of modals.plus.inf
      length(current.mpis)              # the number of modals plus infs from this file

   current.musts <- grep("hw=\"must\"", current.mpis, perl=TRUE, value=TRUE) # find the uses of must in the modals+inf
   infs.after.must <- c(          # make the new version of infs.after.must
      infs.after.must,            # the old version of infs.after.must, plus
      gsub("^.*hw=\"([^\"]+).*$", # the result of replacing everything (but memorizing the second head word, note the greedy .*)
           "\\1",                 # with the memorized second head word
           current.musts,         # in current.musts
           perl=TRUE)             # using Perl-compatible regular expressions
   ) # end of c(...)
} # end of loop

# frequencies of the lemmas/infinitives with must (the sorting isn't really necessary)
freqs.with.must <- sort(table(infs.after.must)) # 440 types, for the whole BNC you would get 1523 types

# frequencies of the lemmas/infinitives with must
sum(freqs.with.must) # 2115 = a+b,                           for the whole BNC you would get 23432

# frequency of all modals plus infinitives (= 'corpus size' for collexeme analysis)
modals.plus.inf # 70257 = a+b+c+d,                           for the whole BNC you would get 527798



# create a vector for the frequencies of lemmas/infinitives with modal verbs in general ...
freqs.overall <- rep(0, length(names(freqs.with.must)))
# ... and give them as names the lemmas/infinitives
names(freqs.overall) <- names(freqs.with.must)

for(i in corpus.files) { # access each corpus file (again)
   cat(basename(i), "\n") # output a 'progress report'

   current.corpus.file <- scan(i, what=character(), sep="\n", quiet=TRUE) # load the i-th/current corpus file
   current.sentences <- tolower(grep("<s n=", # find and to-lower-case the lines with sentence number tags
                                     current.corpus.file, # in the i-th/current corpus file
                                     perl=TRUE,   # using Perl-compatible regular expressions
                                     value=TRUE)) # and return the (whole line) match, not its position index

   current.mpis <- just.matches(                     # matches when you look for
         "<w c5=\"vm0\"[^<]+</w><w c5=\"vvi\"[^<]+", # any modal verbs followed by infinitives
         current.sentences                           # in current.sentences
      ) # end of just.matches(...)
   # once you're done with this script and have understood it, check exercise 8 for chapter 5 #####
   for (j in names(freqs.overall)) { # for each of the 440 verbs attested as an infinitive after must
      current.search.expression <-   # create a search expression by
         paste0("hw=\"", j, "\"")    # pasting it between hw=" and "

      freqs.overall[j] <- freqs.overall[j] + # add to the previous value of freqs.overall for the current verb lemma
         length(                             # the number of
            just.matches(                    # matches of
               current.search.expression,    # that verb lemma
               current.mpis                  # after any modal verb (i.e., not just must)
            ) # end of just.matches(...)
         ) # end of length(...)
   } # end of inner loop

   # instead of the inner loop above (lines 85-92), you could also run the following loop-free code
   # freqs.overall <- freqs.overall +      # add to the previous value of freqs.overall for the current verb lemma
   #    sapply(                            # the result of applying (length, see below)
   #       lapply(                         # to the result of lapplying
   #          paste0("hw=\"",              # the result of pasting hw="
   #                 names(freqs.overall), # in front of the infinitive types attested after must
   #                 "\""),                # before another "
   #          just.matches,                # and using that as the first argument of just.matches
   #          current.mpis                 # with this being the strings searched
   #          ), length                    # the length applied with sapply to the result of lapply
   #    )                                  # end of sapply(...)
} # end of outer loop

# generate the input table for a collexeme analysis
result <- data.frame(                        # put into a data frame called result
   VERB=names(freqs.with.must),              # the verbs attested after must,
   VERBinCORP=freqs.overall,                 # their frequencies after modal verbs in general,
   VERBwithMUST=as.numeric(freqs.with.must), # their frequencies (without names!) after must in particular,
   row.names=NULL)                           # and suppress row names

write.table(result, "../_outputfiles/05_14_must.csv",        # save the data frame into a file
            sep="\t", eol="\n",           # tab-separated and line breaks at the end of rows
            row.names=FALSE, quote=FALSE) # no row names and don't put quotes around strings



# load the collostructional analysis script from my website and start it
source("http://www.linguistics.ucsb.edu/faculty/stgries/teaching/groningen/coll.analysis.r")
# press ENTER
# press ENTER
# enter 1, then press ENTER (to choose collexeme analysis, i.e. collocational strength)
# enter must, then press ENTER
# enter 70257, then press ENTER (to provide the script with the corpus size, i.e. a+b+c+d of all tables like Table 15)
# enter 2115, then press ENTER (to provide the script with the frequency of must, i.e. a+b of all tables like Table 15)
# enter 1, then press ENTER (to choose the p-value of the Fisher-Yates exact test as your association measure)
# enter 4, then press ENTER (to sort the output by that association measure)
# enter 4, then press ENTER (to get 4 decimals in the output)
# press ENTER, then choose the csv file just created
# enter 1, then press ENTER (to save the output into a text file)
# press ENTER
# enter 14_must_out.csv, then press ENTER
