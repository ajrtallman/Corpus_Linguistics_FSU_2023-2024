rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.files
options(warn=1)       # make R show any warnings as soon as they arise in the loop

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

bnc_coca.fiction.files <- dir(rchoose.dir(), full.names=TRUE) # the version of the BNC in the COCA format

# create vectors to collect all adjective and noun lemmas as well as the years
all.adjective.lemmas <- all.noun.lemmas <- all.years <- c()

for(i in seq(bnc_coca.fiction.files)) { # access each corpus file
   cat(basename(bnc_coca.fiction.files[i]), # print the basename of the i/th/current corpus file
       "\t:\t",                         # followed by a tab, a colon, and another tab, followed by
       i/length(bnc_coca.fiction.files),    # the percentage of files dealt with, followed by
       "\n")                            # a line break

   # load the i-th/current corpus file and change it to lower case
   current.file <- tolower(scan(bnc_coca.fiction.files[i], what=character(), sep="\n", quiet=TRUE))
   current.file <- strsplit(current.file, "\t") # split that file up at tab stops

   current.lemmas <- sapply(current.file, "[", 2)  # from the elements of the list current.file, extract all 2nd ones
   current.lemmas[!nzchar(current.lemmas)] <- "NA" # to handle cases without a lemma/form
   current.tags <- sapply(current.file, "[", 3)    # from the elements of the list current.file, extract all 3rd ones

   our.adjective.positions <- intersect( # find the intersections of
      grep("^(fast|quick|rapid|swift)$", current.lemmas, perl=TRUE), # the positions of the four words
      which(current.tags=="aj0")                                      # and the position of adjective tags
   )
   all.noun.positions <- grep("^nn", current.tags, perl=TRUE) # find the positions of noun tags

   # determine the adj and n positions when any of our four adjectives precedes anything tagged as a noun
   our.adj.be4.n.positions <- intersect(our.adjective.positions, all.noun.positions-1)
   n.after.our.adj.positions <- intersect(our.adjective.positions+1, all.noun.positions)

   all.adjective.lemmas <- c(all.adjective.lemmas,                    # add to the collector all.adjective.lemmas
                             current.lemmas[our.adj.be4.n.positions]) # all adjective lemmas before nouns
   all.noun.lemmas <- c(all.noun.lemmas,                              # add to the collector all.noun.lemmas
                        current.lemmas[n.after.our.adj.positions])    # all noun lemmas after our adjectives

   all.years <- c(all.years, rep( # add to the collector all.years the repetitions of
      as.numeric(                               # the numbers that
         just.matches(                          # you find when you search for
            "\\d{4}",                           # sequences of four digits
            basename(bnc_coca.fiction.files[i]) # in the corpus file name
         ) # end of just.matches(...)
      ), # end of as.numeric(...)
      length(current.lemmas[our.adj.be4.n.positions]) # as many times as there are matches in this current file
   )) # end of c(..., rep(...))
} # end of loop

# store the results in a data frame
result1 <- data.frame(
   ADJECTIVES=all.adjective.lemmas,
   NOUNS=all.noun.lemmas,
   YEARS=all.years); order.index <- order(result1$YEAR, result1$ADJECTIVES, result1$NOUNS); summary(result1 <- result1[order.index,])
write.table(result1, file="../_outputfiles/05_15_speed-adj-in-bncascoca1.csv", # and save it into a file
            sep="\t", eol="\n",              # tab-separated and line breaks at the end of rows
            row.names=FALSE, quote=FALSE)    # no row names and don't put quotes around strings

result1.list <- split(result1, # split up the data frame into a list with
                      all.decades <- substr(result1$YEARS, 3, 3)) # a separate data frame for each decade (too little data per year)
result1.list[[2]] <- NULL      # because there are only two data points for the 1940s
names(result1.list)[1] <- "0"  # let's make the 0 stand for 'unknown'

# create a list to collect the top 10 attracted collocates for each adjective lemma in each decade
top.10.collocates <- vector(mode="list", length=length(result1.list))
   names(top.10.collocates) <- names(result1.list)

for(i in seq(top.10.collocates)) { # access each year's data frame
   current.year <- droplevels(result1.list[[i]])[,-3] # and drop unused factor levels as well as the now useles YEAR column
   current.residuals <- chisq.test(table(             # compute a chi-squared test of a table of
      current.year$NOUNS,                             # all noun collocates in this year with
      current.year$ADJECTIVES                         # all adjectives in this year
      ), correct=FALSE)$residuals                     # use no continuity correction and get the residuals

   for(j in 1:4) { # for each of the 4 columns of this table of Pearson residuals
      top.10.collocates[[i]][[colnames(current.residuals)[j]]] <- tail( # get the top residuals
         sort(current.residuals[,j]),                                   # when the residuals are sorted in each column
      10)                                                               # namely the top 10 residuals
   } # end of inner loop (accessing the residuals columns)
} # end of outer loop (accessing each year's results)
top.10.collocates # inspect the result, which we then want to turn into a data frame

temp <- unlist(top.10.collocates) # unlist the just created list and
summary(result2 <- data.frame(    # from that look at the summary of a new data frame that contains
   ADJECTIVES=sub("\\..*", "", sub("^.*?\\.", "", names(temp), perl=TRUE), perl=TRUE), # the adjectives (middle part of the names)
   NOUNS=sub("^.*\\.", "", names(temp), perl=TRUE),            # the nouns (final part of the names)
   DECADES=as.numeric(sub("\\..*", "", names(temp), perl=TRUE)), # the years (first part of the names)
   RESIDUAL=unlist(temp)                                       # the residuals
))

write.table(result2, file="../_outputfiles/05_15_speed-adj-in-bncascoca2.csv", # and save it into a file
            sep="\t", eol="\n",              # tab-separated and line breaks at the end of rows
            row.names=FALSE, quote=FALSE)    # no row names and don't put quotes around strings
