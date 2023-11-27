rm(list=ls(all=TRUE)) # clear memory
file.sizes.ie.exp.percs <- c(0.5, 0.3, 0.2)
word.hits.ie.obs.percs <- c(0.7, 0.2, 0.1)
0.5*sum(abs(word.hits.ie.obs.percs-file.sizes.ie.exp.percs)) # 0.2

# an example of a much more uneven distribution
file.sizes.ie.exp.percs <- c(0.5, 0.3, 0.2)
word.hits.ie.obs.percs <- c(0.1, 0.1, 0.8)
0.5*sum(abs(word.hits.ie.obs.percs-file.sizes.ie.exp.percs)) # 0.6

# an example for a much more even distribution
file.sizes.ie.exp.percs <- c(0.5, 0.3, 0.2)
word.hits.ie.obs.percs <- c(0.49, 0.31, 0.2)
0.5*sum(abs(word.hits.ie.obs.percs-file.sizes.ie.exp.percs)) # 0.01



rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.dir()
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


# define the words for which you want dispersion values
words <- tolower(c("HIV", "keeper", "lively"))
search.expressions <- paste0(">", words, " ?</w>")

# define the corpus files
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# define data structures to collect results
sizes.of.files.in.words <- integer(length=length(corpus.files)) # create a vector for each corpus file's size in words

# create a list that has as many elements as there are words for which we compute DPs (here: 3) and
# where every element has a vector with as many slots as there are files (here: 4049) so that
# we can store each word's frequency in each file
temp <- rep(NA, length(corpus.files))    # create a vector with 1 NA for each corpus file
   names(temp) <- basename(corpus.files) # names the NAs with the corpus files
freqs.of.words <- vector(mode="list", length=length(words)) # create a list with 1 element for each word for which we want DP
   names(freqs.of.words) <- words    # name the list elements with the words for which we want DPs
   "["(freqs.of.words) <- list(temp) # put the vector temp into each list element



for(i in seq(corpus.files)) { # access each corpus file
   cat( # output a progress report to the screen
      i/length(corpus.files), "\n"
      ) # the proportion of files dealt with after this iteration

   current.corpus.file <- scan(corpus.files[i],      # load the i-th/current corpus file
                               what=character(),     # as a character vector
                               sep="\n",             # with linebreaks as separators between vector elements
                               quote="", quiet=TRUE) # no quote characters and no feedback about the number of elements read
   current.corpus.file <- tolower(current.corpus.file) # switch the file to lower case
   current.sentences <- grep("<s n=",             # find the lines that have sentence number tags in them
                             current.corpus.file, # in the i-th/current corpus file
                             perl=TRUE,           # using Perl-compatible regular expressions
                             value=TRUE)          # and return the (whole line) match, not its position index

   for(j in seq(search.expressions)) {  # for each word/search expression
      freqs.of.words[[j]][i] <- length( # store in the i-th slot the number of times of how often
         just.matches(search.expressions[j], # the closing tag of a word is found with exact.matches.2
                      current.sentences)     # in the sentences of the i/th/current file
      )
      # with exact.matches.2, you could do the same like this
      # freqs.of.words[[j]][i] <- length( # store in the i-th slot the number of times of how often
      #    exact.matches.2(search.expressions[j], # the closing tag of a word is found with exact.matches.2
      #                    current.sentences,     # in the sentences of the i/th/current file
      #                    gen.conc.output=FALSE)[[1]] # (suppress the concordance output to save time/memory)
      # )
   } # end of inner for-loop with j as counter

   sizes.of.files.in.words[i] <- length( # store in the i-th slot the sum of how many
      just.matches("</w>", # the closing tag of a word is found
                   current.sentences) # in the sentences of the i/th/current file
   )
   # with exact.matches.2, you could do the same like this
   # sizes.of.files.in.words[i] <- length( # store in the i-th slot the sum of how many
   #    exact.matches.2("</w>", # the closing tag of a word is found with exact.matches.2
   #                    current.sentences, # in the sentences of the i/th/current file
   #                    gen.conc.output=FALSE)[[1]]) # (suppress the concordance output to save time/memory)

} # end of outer for-loop with i as counter



# compute DP
sizes.of.files.in.words.perc <- # compute the expected %s, the file sizes in % of corpus size by
   sizes.of.files.in.words/     # dividing each corpus file's length in words by
   sum(sizes.of.files.in.words) # # the overall corpus size in words

freqs.of.words.perc <- # compute the observed %s, how much in % of each file each word constitutes
   lapply(                     # apply to
      freqs.of.words,          # each element of the list of word frequencies (freqs.of.words)
      function(x) { x/sum(x) } # an inline/anonymous function that converts the frequencies into %s
   )

# set up a vector to compute DPs
DPs <- numeric(length=length(words)); names(DPs) <- words
for(i in seq(DPs)) {                    # for each word for which to compute DP
   DPs[i] <- sum(                        # compute the sum
      abs(                               # of the absolute
         freqs.of.words.perc[[i]] -      # differences of the observed and the
            sizes.of.files.in.words.perc # expected percentages
      ) # end of abs(...)
   )/2                                   # then divide by 2 and save in the relevant slot of DPs
}                                        # end of loop
DPs

plot(DPs ~ log(sapply(freqs.of.words, sum)), # plot the DPs as a function of all words' logged frequencies
     xlim=c(1, 15), ylim=c(0, 1),            # with axes ranging across the whole range of possible values
     xlab="Frequency", ylab="DP",            # with these axis labels
     pch=substr(words, 1, 1)); grid()        # each word is represented by its first letter
# very similar frequencies, more different dispersion values
