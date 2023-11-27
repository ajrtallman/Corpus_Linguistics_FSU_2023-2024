rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.files

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

# define the function yules.measures
yules.measures <- function(input) {
   m1 <- length(input) # the token frequency

   temp <- table(table(input)) # the frequencies of frequencies table (to shorten the definition of m2)
   m2 <- sum(   # the sum of
      "*"(      # the products of
         temp,                     # the frequency with which each word type occurs in the input
         as.numeric(names(temp))^2 # each observed frequency to the power of 2
      ) # end of "*"(...)
   ) # end of sum(...)

   output <- list(                        # generate the output
      "Yule's K"=10000*(m2-m1) / (m1*m1), # Yule's K
      "Yule's I"=(m1*m1) / (m2-m1)        # Yule's I
   ) # end of list(...)
   return(output) # return the output of the function
}   # end of function definition

baseword.files <- rchoose.files() # <_qclwr2/_inputfiles/dat_basewrd*.txt>

BASEWORDLIST <- WORD <- FAMILY <- NUMBER <- vector()

for(i in seq(baseword.files)) {
   cat(basename(baseword.files[i]), "\t:\t", i/length(baseword.files), "\n") # output progress report
   current.file <- readLines(con <- file(baseword.files[i], encoding="ISO-8859-1"), warn=FALSE); close(con) # load i-th base word list
   current.file <- current.file[nzchar(current.file)] # discard empty lines (which some files have, not sure what for)

   BASEWORDLIST <- c(               # make BASEWORDLIST the combination of
      BASEWORDLIST,                 # the old values of BASEWORDLIST the ones from the current file plus
      rep(i, length(current.file))) # the number of the baseword file as many times as there are words in it

   WORD.curr <- gsub(             # create WORD.corr by replacing
      "\t", "",                   # tabstops with nothing
      just.matches("(^|\t)[^ ]+", # from the matches of the strings at the line beginnings (or after a tabstop)
                   current.file)) # in current.file
   WORD <- c(WORD, WORD.curr)     # add to the old values of WORD the ones from the current file
   # Note: this doesn't work in R because PCRE doesn't support variable-length lookbehind
   # WORD.curr <- just.matches("(?<=(^|\t))[^ ]+", current.file)

   # set up a vector to construct the FAMILY words for the current file
   FAMILY.curr <- rep(NA, length(current.file))

   # find the lines where the FAMILY element is the WORD.curr element
   starts.with.word.yes <- grep("^[^\t]",        # find elements that do NOT begin with a tab stop
                                   current.file, # in current.file
                                   perl=TRUE)    # using Perl-compatible regular expressions
   # fill the FAMILY vector with the words that are their own family
   FAMILY.curr[starts.with.word.yes] <- WORD.curr[starts.with.word.yes]

   # find the lines where the FAMILY element is the most recent WORD.curr element
   starts.with.word.no  <- grep("^\t",           # find elements that begin with a tab stop
                                   current.file, # in current.file
                                   perl=TRUE)    # using Perl-compatible regular expressions

   # now we need to find for every still missing FAMILY.curr entry (the remaining NAs)
   # the most recent FAMILY.curr entry that is not NA
   # first, we generate a matrix of differences: every position that still needs a FAMILY entry minus
   # every position that already has a FAMILY entry
   position.differences <- sapply( # create a matrix position.differences by
      starts.with.word.no,         # applying to the positions that still need a FAMILY entry
      "-",                         # the operation "subtract"
      starts.with.word.yes)        # namely subtract the positions of all already entered FAMILY entries
   # this is in a loop but for purely didactic reasons, we label all rows and columns:
      rownames(position.differences) <-    # give position.differences rownames that are
         FAMILY.curr[starts.with.word.yes] # the FAMILY entries that we already have and from which we assign the as yet missing ones
      colnames(position.differences) <-    # give position.differences colnames that are
         starts.with.word.no               # the slots in FAMILY.curr that still need to be assigned entries

   # change all values<1 to large positive ones so that we can use which.min
   position.differences[position.differences<1] <- # make every value in the matrix that is 1 or smaller
      max(position.differences)                    # the largest value in position.differences

   # now we find for each slot for which we still need a FAMILY entry
   # the most recent FAMILY that we DO already have:
   entries.to.be.inserted <- rownames(position.differences)[ # retrieve those row names of position.differences where
      apply(position.differences,                            # if you look at the matrix position.differences
            2,                                               # columnwise
            which.min)                                       # the differences (to the last-used FAMILY entry) are smallest
   ]
   FAMILY.curr[starts.with.word.no] <- entries.to.be.inserted # put those entries into the still NA slots of FAMILY.curr
   FAMILY <- c(FAMILY, FAMILY.curr) # add to the old values of FAMILY.curr the ones from the current file

   NUMBER.curr <- just.matches("\\d+$",      # create NUMBER.curr by extracting line-final digits
                               current.file)
   NUMBER <- c(NUMBER, as.numeric(NUMBER.curr)) # add to the old values of NUMBER the new ones from the current file
} # end of loop

all <- data.frame(BASEWORDLIST, FAMILY, WORD, NUMBER) # merge all results into a new data frame
write.table(all, file="../_outputfiles/05_25_lfp-basewords.csv",            # and save it into a file
            sep="\t", eol="\n",                       # tab-separated and line breaks at the end of rows
            row.names=FALSE, quote=FALSE)             # no row names and don't put quotes around strings



# define the paths to the two text files we want to compare
text.files <- rchoose.files() # <_qclwr2/_inputfiles/corp_perl.txt> and <_qclwr2/_inputfiles/corp_python.txt>

# generate a list to store the results
lists.of.tables <- list()

for(i in text.files) { # access each file
   text.file <- toupper(scan(i, what=character(), sep="\n", quiet=TRUE)) # load the i-th/current corpus file

   # split up into words (simplistically) and tabulate
   word.tokens <- unlist(  # create a vector word.tokens by unlisting
      strsplit(text.file,  # the result of splitting up the text file
               "\\W+",     # at occurrences of 1+ non-word characters
               perl=TRUE)) # using Perl-compatible regular expressions
   word.tokens <- word.tokens[nzchar(word.tokens)] # remove empty character strings
   sorted.freq.list <- sort(table(word.tokens), decreasing=TRUE) # create a sorted frequency list

   # define short and transparent vector names for ease of coding
   word.types <- names(sorted.freq.list)
   type.freqs <- as.numeric(sorted.freq.list)

   # compute the indices of vocabulary diversity
   lex.richness <- yules.measures(word.tokens)

   # for each word in the text files, retrieve the number of its baseword list file
   list.of.each.type <- BASEWORDLIST[ # put into list.of.each.type the values of BASEWORDLIST
      match(word.types,               # where the word types from the perl/python file matches
            WORD)]                    # a word in the baseword data frame

   # for each word in the text files, retrieve its FAMILY in the baseword list files
   family.of.each.type <- FAMILY[ # put into family.of.each.type the values of FAMILY
      match(word.types,           # where the word types from the perl/python file matches
            WORD)]                # a word in the baseword data frame

   # We generate a vector fam.freqs that contains for each word
   # the summed frequency of all its family members in the corpus file.
   family.freq.of.each.type <- tapply( # apply
      type.freqs,                      # to the frequencies of all word types in the current perl/python file
      family.of.each.type,             # a grouping by the word families
      sum)                             # and sum those up

   # However, we don't need the sums just once and in alphabetical order. First, we rather need some sums more than once because more than one member of a family may have been found in the corpus. For example, we will need the family frequency of a/an once for "a" in the frequency list and once again for "an".
   # Second, we need all these frequencies in the same order as all the other data, i.e., the order following from the frequency based sorting a few lines above. Thus, we now need match again. With match(families, names(fam.freqs)) we retrieve for each family the position of the summed corpus frequency in fam.freqs, and then we can use subsetting to not just get the position, but the summed frequency itself.
   family.freq.of.each.type <- family.freq.of.each.type[ # retrieve the family frequency of each type in the corpus files
      match(family.of.each.type,                         # from the positions of the family of each type
            names(family.freq.of.each.type))]            # in the names of the family frequency of each type

   # change into data frame
   result <- data.frame(WORDTYPE=word.types,
                        WORDTYPEFREQ=type.freqs,
                        WORDTYPEFAMILY=family.of.each.type,
                        WORDTYPEFAMILYFREQ=family.freq.of.each.type,
                        WORDTYPELIST=list.of.each.type)

   # In what follows below, we first extract the name of the programming language from the name of the file and then paste together a file name to save the data frame into an appropriately named results file.
   # save the table
   programming.language <- unlist(strsplit(basename(i), "[_\\.]", perl=TRUE))[2] # get the programming language studied
   output.file.name <- paste("../_outputfiles/05_25_lfp-", programming.language, ".csv", sep="") # create a file name to save result
   write.table(result,                       # save the data frame for this programming language
               output.file.name,             # into the corresponding file
               sep="\t", eol="\n",           # tab-separated and line breaks at the end of rows
               row.names=FALSE, quote=FALSE) # no row names and don't put quotes around strings

   # In order to facilitate the access to the data later,
   # we also store the absolute frequencies of types per base word list - the lexical frequency profile - into a list.
   lists.of.tables[[programming.language]] <- table(
      list.of.each.type, # count how many types are from which baseword list
      exclude=FALSE)     # include the unattested cases (i.e. NAs) in counts

   # generate barplots
   output.graph.name <- paste("../_outputfiles/05_25_lfp-", programming.language, ".png", sep="") # create a file name to save graphs
   list.percentages <- round(                             # store in list.percentages the rounded
      prop.table(lists.of.tables[[programming.language]]) # % of how many word types are from each baseword list
      , 3) # use 3 decimals only
   png(output.graph.name, height=6, width=12, # save into the above file name a png of dimensions 6*6
       units="in", res=150)                  # where dimensions are inches and the resolution is publication-ready
      barplot(list.percentages,              # a bar plot of the % of how many word types are from each baseword list
              space=0,                       # with no spaces between bars
              ylim=c(0, 1),                  # and y-axis limits from 0 to 1 (100%)
              xlab="List", ylab="Percentage of words in a list") # and axis labels
      text(seq(list.percentages)-0.5, # plot above each bar
           list.percentages,          # at the height of the bars
           list.percentages,          # the height of the bars
           cex=0.8, pos=3)            # in smaller font (by 20%) and slightly above the above-defined y-coordinates
      text(mean(par("usr")[1:2]), 0.8, # text into the middle of the plot (horizontally) and at y=0.8
           paste("Yule's K",           # the measure Yule's K
                 round(lex.richness[[1]], 4), sep=" = "), # rounded and with some annotation
           pos=1) # also add Yule's K, into               # and in fact a bit below 0.8
      text(mean(par("usr")[1:2]), 0.8, # text into the middle of the plot (horizontally) and at y=0.8
           paste("Yule's I",           # the measure Yule's K
                 round(lex.richness[[2]], 4), sep=" = "), # rounded and with some annotation
           pos=3)                                         # and in fact a bit above 0.8
   dev.off() # save the file
} # end of loop



# compute a chi-squared test for goodness of fit to check whether
# the baseword list frequencies of the perl file are significantly different from
# those of the Python file (see SFLWR2: Section 4.1.1.2)
(test <- chisq.test(lists.of.tables$perl,
                         p=(lists.of.tables$python/
                               sum(lists.of.tables$python))))
# ** but it seems like a very weak effect
lists.of.tables$perl
round(test$expected, 3)
round(test$residuals, 3)



# excursus:
Brown <- c(13, 3, 1); LOB <- c(9, 4, 6)
names(Brown) <- names(LOB) <- c("horrible", "horrifying", "horrid")

# testing whether the two vectors of frequencies differ from each other:
chisq.test(rbind(Brown, LOB), correct=TRUE)

# testing whether the Brown corpus differs from the LOB corpus:
chisq.test(Brown, p=LOB/sum(LOB), correct=TRUE)
