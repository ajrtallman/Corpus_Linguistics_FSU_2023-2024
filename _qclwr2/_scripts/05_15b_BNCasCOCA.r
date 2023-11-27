rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.dir()
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

corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)
dir.create("../_outputfiles/05_15_BNCasCOCA")
problem <- rep(FALSE, length(corpus.files))

for(i in seq(corpus.files)) { # access each corpus file
   cat( # output a progress report to the screen
      i/length(corpus.files), "\n"
      ) # the proportion of files dealt with after this iteration

   current.corpus.file <- scan(corpus.files[i],      # load the i-th/current corpus file
                               what=character(),     # as a character vector
                               sep="\n",             # with linebreaks as separators between vector elements
                               quote="", quiet=TRUE) # no quote characters and no feedback about the number of elements read
   current.sentences.1 <- grep("<s n=",             # find the lines that have sentence number tags in them
                               current.corpus.file, # in the i-th/current corpus file
                               perl=TRUE,           # using Perl-compatible regular expressions
                               value=TRUE)          # and return the (whole line) match, not its position index
   current.sentences.2 <- gsub("<(?!/?m?w ?)[^>]+>", "", current.sentences.1, perl=TRUE)
   current.words <- just.matches("<(m?w) c5.*?</\\1>", current.sentences.2)

   current.forms.1 <- gsub("<w .*?>", "", current.words, perl=TRUE)
   current.forms.2 <- gsub("</w>", "", current.forms.1, perl=TRUE)
   current.forms.3 <- whitespace(gsub("<.*?>", "", current.forms.2, perl=TRUE))

   current.tags.1 <- just.matches("(?<=c5=\")[^\"]+", current.words, vectorize=FALSE)
   current.tags.2 <- as.character(sapply(current.tags.1, "[", 1))

   current.lemmas.1 <- just.matches("(?<=hw=\")[^\"]+", current.words, vectorize=FALSE)
   current.lemmas.2 <- as.character(sapply(current.lemmas.1, paste, collapse=" "))

   current.year <- just.matches("(?<=<creation date=\")\\d+", current.corpus.file)

   if(
      length(current.forms.3)==length(current.tags.2)
         &
      length(current.tags.2)==length(current.lemmas.2)
   ) {
      output <- paste(current.forms.3, current.lemmas.2, current.tags.2, sep="\t")
      cat("FORM\tLEMMA\tTAG", output,
          file=paste0(
             "../_outputfiles/05_15_BNCasCOCA/",
             gsub("\\.xml$", paste0("_", current.year, ".txt"),
                  basename(corpus.files[i]),
                  ignore.case=TRUE, perl=TRUE)
          ),
          sep="\n")
   } else {
      problem[i] <- TRUE
   }
}
length(corpus.files[problem])
corpus.files[problem]
