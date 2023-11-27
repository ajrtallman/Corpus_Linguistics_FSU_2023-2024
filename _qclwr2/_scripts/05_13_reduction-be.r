rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for tk_choose_dir
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
all.bes <- all.verbs <- character()

for(i in corpus.files) { # access each file
   cat( # output a progress report to the screen
      basename(i), "\t",    # the file that will be loaded on this iteration and a tab stop
      length(all.bes), "\n" # and the number of some form of _to be_ + gerund found so far
   ) # end of cat(...)

   current.corpus.file <- tolower(scan(i, what=character(), sep="\n", quiet=TRUE)) # load & to-lower-case the i-th/current corpus file

   # check if the corpus file contains spoken data
   if(!                          # if not
      any(                       # there is any one occurrence of
      grepl("<stext ",           # "<stext"
            current.corpus.file) # in the character vector current.corpus.file
       )                         # end of any(...)
   ) { next }                    # then go to the next corpus file right away

   # retrieve only the lines with sentences in them (i.e., discard the header, utterance tags, etc.)
   current.sentences.1 <- grep("<s n=",             # find the lines that have sentence number tags in them
                               current.corpus.file, # in the i-th/current corpus file
                               perl=TRUE,           # using Perl-compatible regular expressions
                               value=TRUE)          # and return the (whole line) match, not its position index

   # get rid of everything that's not a word and its annotation
   current.sentences.2 <- gsub("<(?!w )[^<]*",      # find all occurrences of stuff not tagged as a word and its tag
                               "",                  # replace them by nothing
                               current.sentences.1, # in the vector current.sentences.1
                               perl=TRUE)           # using Perl-compatible regular expressions

   current.bes.be4.v.g <- just.matches( # create a vector current.bes.be4.v.g containing matches of
      "<w [^>]+hw=\"(be|was)\"[^>]*?>[^<]*<w c5=\"vvg\"[^<]+", # forms of _to be_ followed by gerunds
      current.sentences.2)                                     # in current.sentences.2

   # with exact.matches.2, you could do the same like this
   # current.bes.be4.v.g <- exact.matches.2( # create a vector current.bes.be4.v.g containing matches of
   #    "<w [^>]+hw=\"(be|was)\"[^>]*?>[^<]*<w c5=\"vvg\"[^<]+", # forms of _to be_ followed by gerunds
   #    current.sentences.2,                                     # in current.sentences.2
   #    gen.conc.output=FALSE)[[1]]          # do not generate concordance output and just retain the exact matches

   all.bes <- c(  # create a new version of all.bes that contains
      all.bes,    # the old version of all.bes plus
      whitespace( # a whitespace-stripped version of
         just.matches(           # the search for
            "(?<=>).*?(?=<)",    # what's between a ">" and a "<"
            current.bes.be4.v.g) # in current.bes.be4.v.g
      ) # end of whitespace(...)
   ) # end of c(...)

   # with exact.matches.2, you could do the same like this
   # all.bes <- c(  # create a new version of all.bes that contains
   #    all.bes,    # the old version of all.bes plus
   #    whitespace( # a whitespace-stripped version of
   #       exact.matches.2(        # the search for
   #          "(?<=>).*?(?=<)",    # what's between a ">" and a "<"
   #          current.bes.be4.v.g, # in current.bes.be4.v.g
   #          gen.conc.output=FALSE)[[1]] # do not generate concordance output and just retain the exact matches
   #    ) # end of whitespace(...)
   # ) # end of c(...)

   all.verbs <- c( # create a new version of all.verbs that contains
      all.verbs,   # the old version of all.verbs plus
      whitespace(  # a whitespace-stripped version of
         gsub("^.*>", "",          # what you get when you delete everything till the last ">"
              current.bes.be4.v.g, # in current.bes.be4.v.g
              perl=TRUE)           # using Perl-compatible regular expressions
      ) # end of whitespace(...)
   ) # end of c(...)
} # end of loop

# compile the results in a data frame result.of.loop
result.of.loop <- data.frame(
   BE=all.bes,
   VERB=all.verbs,
   REDUCED=ifelse(         # add a column REDUCED that
      grepl("'", all.bes), # checks for the presence of ' in the be-form and
      "yes", "no")         # calls something 'reduced/yes' if there is one, and "'full/no' if there isn't
)
save(result.of.loop, file="../_outputfiles/05_13_reduction-be.RData")

reductions <- table(result.of.loop$VERB,     # cross-tabulate the verbs
                    result.of.loop$REDUCED)  # with the reduction indicator
reductions.perc <- prop.table(reductions, 1) # and generate a table of reduction proportions as row %s

order.index <- order(rowSums(reductions,           # create an order.index using the verbs' joint freq
                             reductions.perc[,2]), # and the reduction percentages
                     decreasing=TRUE)              # in descending order

reductions <- reductions[order.index,]           # create a new reductions by reordering the rows of the old one
reductions.perc <- reductions.perc[order.index,] # create a new reductions.perc by reordering the rows of the old one

head(reductions, 20)      # check ...
head(reductions.perc, 20) # ... the result



# approach 1: frequency of reduction of to be as a function of the frequency of a gerund after to be
plot(log(rowSums(reductions)), # plot the frequencies of the gerunds after to be on the x-axis
     reductions.perc[,2],      # plot the reduction %s on the y-axis
     xlab="Logged frequency of gerunds after forms of to be", # with this x-axis label
     ylab="Percentage of reduced forms of to be",             # with this y-axis label
     type="n") # but don't actually plot anything
   text(log(rowSums(reductions)), # plot at the frequencies of the gerunds after to be on the x-axis and
     reductions.perc[,2],         # at the reduction %s on the y-axis
     rownames(reductions),
     cex=2/3, font=3); grid()     # in a smaller (by 1/3) font in italics; add a grid
   abline(h=(colSums(reductions)/ # add a horizontal line at the overall %
                sum(colSums(reductions)))[2], lty=2) # of reductions (dashed)
   lines(lowess(reductions.perc[,2] ~ log(rowSums(reductions))), # summarize the trend
         lwd=3, col="red")         # with a thick red line
cor.test(log(rowSums(reductions)), # compute a correlation between the gerunds frequencies after forms of to be and
         reductions.perc[,2],      # their reduction percentages
         method="kendall")         # namely Kendall's tau
# *** but weak positive correlation (see SFLWR2: Section 4.4)



# approach 2: frequency of reduction of to be as a function of the frequency of a gerund in general
# load the data frame with Kilgarriff's frequency data from <corp_bnc_sgml_freql.txt>
freqs <- read.table(file.choose(), header=TRUE, # read in a table with a header
                    sep=" ",                    # where spaces separate columns
                    comment.char="", quote="")  # without comment or quote characters
str(freqs)  # check the import result
head(freqs) # look at the top of the data frame

# retrieve the relevant parts of the data frame
freqs.vvg <- droplevels( # create a data frame freqs.vvg that contains only the used levels of
   freqs[                # what remains of freq when you choose only those rows
      grepl("vvg",       # where grepl finds "vvg"
            freqs$POS),  # in the POS tag column
   ] # end of subsetting
) # end of droplevels(...)
str(freqs.vvg)  # check ...
head(freqs.vvg) # ... the result

# housekeeping
rm(freqs) # we remove freqs from the workspace since there's no need to keep that huge data frame in working memory

# since some gerunds (such as "zooming" or "yodelling") might be represented with more than one tag,
freqs.vvg <- tapply(freqs.vvg$FREQUENCY,
                    freqs.vvg$WORD,
                    sum)

# check whether the old frequency list file covers all verbs found in the BNC XML
(missing.vs <- setdiff(rownames(reductions), names(freqs.vvg)))
(missing.where <- match(missing.vs, rownames(reductions)))
# excellent: all but just two verbs we found in the XML version are covered by the freqlist file
freqs.vvg <- freqs.vvg[rownames(reductions.perc)] # retrieving overall frequencies of those gerunds that follow to be
freqs.vvg[missing.where] <- 1                 # set the frequencies of the two missing verbs to 1 (for later logging)
names(freqs.vvg[missing.where]) <- missing.vs # name those frequencies by the missing verbs

plot(log(freqs.vvg),      # plot the frequencies of the gerunds after to be on the x-axis
     reductions.perc[,2], # plot the reduction %s on the y-axis
     xlab="Logged frequency of gerunds in general", # with this x-axis label
     ylab="Percentage of reduced forms of to be",   # with this y-axis label
     type="n")             # but don't actually plot anything
   text(log(freqs.vvg),    # plot at the overall frequencies of the gerunds on the x-axis
     reductions.perc[,2],  # at the reduction %s on the y-axis
     rownames(reductions), # the gerunds
     cex=2/3, font=3); grid() # in a smaller (by 1/3) font in italics; add a grid
   abline(h=(colSums(reductions)/ # add a horizontal line at the overall %
                sum(colSums(reductions)))[2], lty=2) # of reductions (dashed)
   lines(lowess(reductions.perc[,2] ~ log(freqs.vvg)), # summarize the trend
         lwd=3, col="red")    # with a thick red line
cor.test(log(freqs.vvg),      # compute a correlation between the gerunds overall frequencies
         reductions.perc[,2], # their reduction percentages
         method="kendall")    # namely Kendall's tau
# *** but VERY weak positive correlation (see SFLWR2: Section 4.4)
