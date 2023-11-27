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

# define the function whitespace
whitespace <- function (input.vector, leadtrail=TRUE, reduce=TRUE, empties=TRUE) {
   if(reduce) { # if reduce is TRUE
      input.vector <- gsub(" {2,}", " ", input.vector, perl=TRUE) # replace all occ of 2+ spaces by 1 space
   }
   if(leadtrail) { # if leadtrail is TRUE
      input.vector <- gsub("(^ +| +$|)", "", input.vector, perl=TRUE) # replace leading/trailing spaces by nothing
   }
   if(!empties) { # if empties is FALSE
      input.vector <- input.vector[nzchar(input.vector)] # retain only non-empty results
   }
   return(input.vector) # return the function's output to the user
} # end of function

# define the directory where the BNC files are located and get the file names
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# prepare collectors and results vectors
all.ic.N.types <- all.ical.N.types <- character()

for(i in corpus.files) { # access each file
   cat( # output a progress report to the screen
      basename(i), "\t", # the file that will be loaded on this iteration and a tab stop
      length(all.ic.N.types), "\t",  # the number of N types after -ic adjectives identified so far
      length(all.ical.N.types), "\n" # the number of N types after -ical adjectives identified so far
      )

   current.corpus.file <- tolower(scan(i, what=character(), sep="\n", quiet=TRUE)) # load the i-th/current corpus file and switch it to lower case

   # retrieve only the lines with sentences in them (i.e., discard the header, utterance tags, etc.)
   current.sentences.1 <- grep("<s n=",             # find the lines that have sentence number tags in them
                               current.corpus.file, # in the i-th/current corpus file
                               perl=TRUE,           # using Perl-compatible regular expressions
                               value=TRUE)          # and return the (whole line) match, not its position index

   # get rid of everything that's not a word and its annotation
   current.sentences.2 <- gsub("<(?!w )[^<]*", # find all occurrences of stuff that's not tagged as a word and its tag
                               "",             # replace them by nothing
                               current.sentences.1, # in the vector current.sentences.1
                               perl=TRUE)           # using Perl-compatible regular expressions

   curr.ic.N.tokens <- just.matches( # create a vector curr.ic.N.tokens by finding
      "(?<=pos=\"adj\">)[^<]+ic ?<w [^>]+pos=\"subst\">[^<]+", # adjectives ending in ic followed be a substantive
      current.sentences.2)                                     # in current.sentences.2
   curr.ical.N.tokens <- just.matches( # create a vector curr.ical.N.tokens by finding
      "(?<=pos=\"adj\">)[^<]+ical ?<w [^>]+pos=\"subst\">[^<]+", # adjectives ending in ical followed be a substantive
      current.sentences.2)          # in current.sentences.2

   # with exact.matches.2, you could do the same like this
   # curr.ic.N.tokens <- exact.matches.2( # create a vector curr.ic.N.tokens by finding
   #    "(?<=pos=\"adj\">)[^<]+ic ?<w [^>]+pos=\"subst\">[^<]+", # adjectives ending in ic followed be a substantive
   #    current.sentences.2,        # in current.sentences.2
   #    gen.conc.output=FALSE)[[1]] # do not generate concordance output and just retain the exact matches
   # curr.ical.N.tokens <- exact.matches.2( # create a vector curr.ical.N.tokens by finding
   #    "(?<=pos=\"adj\">)[^<]+ical ?<w [^>]+pos=\"subst\">[^<]+", # adjectives ending in ical followed be a substantive
   #    current.sentences.2,          # in current.sentences.2
   #    gen.conc.output=FALSE)[[1]]   # do not generate concordance output and just retain the exact matches

   curr.ic.N.tokens <- whitespace( # create a new vector curr.ic.N.tokens by stripping of whitespace
      gsub("^.*>", "",             # from the N that  remains after deleting everything till the final >
           curr.ic.N.tokens,       # in curr.ic.N.tokens
           perl=TRUE)              # using Perl-compatible regular expressions
   ) # end of whitespace(...)
   curr.ical.N.tokens <- whitespace( # create a new vector curr.ical.N.tokens by stripping of whitespace
      gsub("^.*>", "",               # from the N that remains after deleting everything till the final >
           curr.ical.N.tokens,       # in curr.ical.N.tokens
           perl=TRUE)                # using Perl-compatible regular expressions
   ) # end of whitespace(...)

   all.ic.N.types <- unique(   # make a new version of all.ic.N.types that contains only the unique Ns
      c(all.ic.N.types,        # from the previously collected ones
        curr.ic.N.tokens)      # and those from the current file
   ) # end of unique(...)
   all.ical.N.types <- unique( # make a new version of all.ic.N.types that contains only the unique Ns
      c(all.ical.N.types,      # from the previously collected ones
        curr.ical.N.tokens)    # and those from the current file
   ) # end of unique(...)
} # end of loop
result.of.loop <- list(ic=all.ic.N.types, ical=all.ical.N.types)
save(result.of.loop, file="../_outputfiles/05_12_ic-ical-collocates.RData")



# load the data frame with Kilgarriff's frequency data from <_qclwr2/_inputfiles/corp_bnc_sgml_freql.txt>
freqs <- read.table(file.choose(), header=TRUE, # read in a table with a header
                    sep=" ",                    # where spaces separate columns
                    comment.char="", quote="")  # without comment or quote characters
str(freqs)  # check the import result
head(freqs) # look at the top of the data frame

# retrieve the relevant parts of the data frame
freqs.nn <- droplevels( # create a data frame freqs.nn that contains only the used levels of
   freqs[               # what remains of freq when you choose only those rows
      grepl("^nn",      # where grepl finds "nn" as the beginning of the tag
            freqs$POS), # in the WORD column
   ] # end of subsetting
) # end of droplevels(...)
str(freqs.nn)  # check ...
head(freqs.nn) # ... the result

# housekeeping
rm(freqs) # we remove freqs from the workspace since there's no need to keep that huge data frame in working memory

# since some nouns (such as "kiss" or "mess") might be represented with more than one tag,
# we first sum all frequencies for all nouns across POS tags
nn.freqs <- tapply(    # create a data structure nn.freqs by applying
   freqs.nn$FREQUENCY, # to the frequencies of all nouns
   freqs.nn$WORD,      # a grouping by the nouns
   sum)                # and then applying the function sum to each group

summary(ic.n.freqs <-                # create a vector ic.n.freqs (and then summarize it) by
           nn.freqs[all.ic.N.types]) # retrieving the overall frequencies of those nouns that follow ic-adjectives
summary(ical.n.freqs <-nn.freqs[all.ical.N.types]) # same for ical-adjectives



# compare the ic-N-collocate frequencies to those of ical with a boxplot (see Section 4.3.1, and SFLWR2: Section 3.2.2.1)
boxplot(ic.n.freqs, ical.n.freqs,
        notch=TRUE, log="y",
        names=c("ic", "ical")); grid()
# compute a Wilcoxon test to check whether the ic-N-collocate frequencies are significantly different from those of -ical (see Section 4.3.2, and SFLWR2: Section 4.3.2.3)
wilcox.test(ic.n.freqs, ical.n.freqs)
# the collocate frequencies of the ical-adjectives are significantly higher than those of the ic-adjectives

plot(ecdf(log(ic.n.freqs)), # plot the empirical cumulative distribution of the ic-N-collocate frequencies
     xlab="Logged frequency of N collocate", ylab="Cumulative percentage", # with these axis labels
     main="", sub="(-ic = blue, -ical = red)", # with no main heading and this text under the x-axis label
     verticals=TRUE, col="blue") # vertical connection lines and in blue
   plot(ecdf(log(ical.n.freqs)), verticals=TRUE, col="red", add=TRUE) # add the ical-N-collocate frequencies
   grid() # add a grid

# compute a Kolmogorov-Smirnov test to check whether the ic-N-collocate frequencies are significantly different from those of -ical (see SFLWR2: Section 4.1.2.1)
ks.test(log(ic.n.freqs),   # compare the ecdf of the frequencies of the collocates of -ic adjectives
        log(ical.n.freqs)) # to that of the collocates of -ical adjectives
