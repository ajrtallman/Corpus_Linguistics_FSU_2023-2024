rm(list=ls(all=TRUE)) # clear memory
library(dplyr) # load the library for %>%
library(rChoiceDialogs) # load the library for rchoose.files()

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

# define the function age.converter
age.converter <- function (x) {
   if(is.numeric(x)) { # if the input to age.converter is a number, then
      year <- floor(x) # compute the floor of x, i.e. the largest integer <= x
      month <- round(floor((x - year) * 12))           # from the remaining decimal, compute a number of months
      day <- round((x - (year + (month/12))) * 365)    # from the remaining decimal, compute a number of days
      return(c(Year = year, Month = month, Day = day)) # return a named numeric vector
   } else {            # if the input to age.converter is a number, then
      parts <- as.numeric(unlist(  # get the numbers from a vectorized (with unlist) list
         strsplit(as.character(x), # resulting from splitting the the input (made into a character string)
                  "\\D+",          # at every character that's not a digit
                  perl=TRUE)))     # using Perl-compatible regular expressions
      if(length(parts)==2) { parts <- c(parts, 0) } # if the input contained no days, add a zero for those
      return(sum(parts[1], parts[2]/12, parts[3]/365)) # return a numeric vector that sumes up years, months, and days
   } # end of conditional expression
} # end of function

# define the corpus files (in <_qclwr2/_inputfiles/corp_26_eve/>)
chat.files <- rchoose.files()

# create data structures to collect MLUs and TTRs for each chat line
lus <- ttrs <- vector(mode="list", length=length(chat.files))

# create data structures to collect MLUs and TTRs for each chat file
mlus <- mttrs <- vector(length=length(chat.files))

for(i in seq(chat.files)) { # access each corpus file
   cat(i/length(chat.files), "\n") # output a progress report

   current.chat.file.01 <- scan(chat.files[i], what=character(), sep="\n", quiet=TRUE) # load the i-th corpus file

   current.chat.file.02 <- paste(current.chat.file.01, # paste each corpus file together into one string
                                 collapse="_dReck_")   # with line breaks now represented as a string that shows up in no file

   current.chat.file.03 <- gsub("_dReck_\\s+", " ",   # replace formerly line-initial whitespace by just a space
                                current.chat.file.02, # in current.chat.file.02
                                perl=TRUE)            # using Perl-compatible regular expressions

   current.chat.file.04 <- unlist(strsplit(current.chat.file.03, # split current.chat.file.03 up where
                                           "_dReck_"))           # line breaks used to be

   current.chat.file.05 <- grep("^\\*CHI:",           # look for line-initial CHI annotation (for the child's utterances)
                                current.chat.file.04, # in current.chat.file.04
                                perl=TRUE,            # using Perl-compatible regular expressions
                                value=TRUE)           # retrieve the matches, not their positions

   current.chat.file.06 <- sub("^.*?\t", "",         # replace everything till and including the first tab (line-initial annotation)
                               current.chat.file.05, # in current.chat.file.05
                               perl=TRUE)            # using Perl-compatible regular expressions

   current.chat.file.07 <- gsub(" *\\[.*?\\] *", " ", # replace everything between square brackets by a space
                                current.chat.file.06, # in current.chat.file.06
                                perl=TRUE)            # using Perl-compatible regular expressions

   current.chat.file.08 <- gsub("[^a-zA-Z ]+", "",    # delete everything that's not letters or spaces
                                current.chat.file.07, # in current.chat.file.07
                                perl=TRUE)            # using Perl-compatible regular expressions

   current.chat.file.09 <- strsplit(current.chat.file.08, # split up current.chat.file.08
                                    " +",                 # at every occurrence of 1+ spaces
                                    perl=TRUE)            # using Perl-compatible regular expressions

   current.chat.file.10 <- sapply(current.chat.file.09, # sapply to current.chat.file.09
                                  grep,                 # the function grep
                                  pattern="\\w",        # looking for at least one word character
                                  perl=TRUE,            # using Perl-compatible regular expressions
                                  value=TRUE)           # retrieve the matches, not their positions

   # excursus: this is how nicely the above could be written using dplyr's chaining
   # current.chat.file.10 <-
   #    scan(chat.files[i], what=character(), sep="\n", quiet=TRUE) %>% # current.chat.file.01
   #    paste(collapse="___")                                       %>% # current.chat.file.02
   #    gsub("___\\s+", " ", ., perl=TRUE)                          %>% # current.chat.file.03
   #    strsplit("___") %>% unlist                                  %>% # current.chat.file.04
   #    grep("^\\*CHI:", ., perl=TRUE, value=TRUE)                  %>% # current.chat.file.05
   #    sub("^.*?\t", "", ., perl=TRUE)                             %>% # current.chat.file.06
   #    gsub(" *\\[.*?\\] *", " ", ., perl=TRUE)                    %>% # current.chat.file.07
   #    gsub("[^a-zA-Z ]+", " ", ., perl=TRUE)                      %>% # current.chat.file.08
   #    strsplit(" +", perl=TRUE)                                   %>% # current.chat.file.09
   #    sapply(grep, pattern="\\w", perl=TRUE, value=TRUE)              # current.chat.file.10

   lus[[i]] <- sapply(current.chat.file.10, # apply to every element of current.chat.file.10 (i.e., every cleaned utterance)
                       length)              # the function length (to count the number of word tokens)
   ttrs[[i]] <- sapply(current.chat.file.10,   # apply to every element of current.chat.file.10 (i.e., every cleaned utterance)
                       function (qwe)          # an inline/anonymous function that
                          length(unique(qwe))/ # divides the number of unique word types per utterance by
                          length(qwe))         # the number of word tokens per utterance

   mlus[i] <- mean(lus[[i]], na.rm=TRUE, trim=0.05) # mlus[i] <- median(lus[[i]], na.rm=TRUE) # also possible
   mttrs[i] <- length(unique(current.chat.file.10)) / # store the result of dividing the number of types per file
      length(current.chat.file.10)                    # by the number of tokens per file
      # the alternative of mttrs[i] <- mean(ttrs[[i]], na.rm=TRUE, trim=0.05) yields useless results, try it out

   name.of.kid <- just.matches("(?<=CHI )[^ ]+",     # retrieve non-spaces after "CHI "
                               current.chat.file.01) # in current.chat.file.01
   age.of.kid.line <- just.matches("@ID.*?CHI\\|[^|]+",  # retrieve the line that contains the child's age
                                   current.chat.file.01) # in current.chat.file.01
   age.of.kid <- sub(".*\\|", "",     # delete everything till and including the last |
                     age.of.kid.line, # in age.of.kid.line
                     perl=TRUE)       # using Perl-compatible regular expressions

   names(lus)[[i]] <- names(ttrs)[[i]] <- names(mlus)[i] <- names(mttrs)[i] <- # give the current list/vector parts
      paste(name.of.kid, age.of.kid, # the combination of the name and the age of the child
               sep="_")              # separated by an underscore
} # end of loop

mlus  # inspect the (trimmed) mean lengths of utterances per file
mttrs # inspect the type-token ratios per file

sapply(lus, summary)  # get a numerical summary of the utterance lengths in each corpus file
sapply(ttrs, summary) # get a numerical summary of the type-token ratios per utterance of each corpus file



# statistical analysis and visualization
# compute Shapiro-Wilk tests to check whether the utterance lengths are compatible with normal distributions (see SFLWR2: Section 4.1.1.1)
sapply(lus,          # apply to the utterance lengths of each corpus file
       shapiro.test) # the function shapiro.test (to test for deviations from normality
sum(
   sapply(lus, shapiro.test)[
      seq(2, 78, 4)]
   >=0.05
)
# none of the distributions is compatible with the assumption that it might be normal

# compute U-tests to check whether the utterance lengths are different in all file-by-file comparisons
# create a matrix to collect the results of comparing each file's utterance lengths to each file's utterance lengths
# we set up a symmetric 20x20 matrix, which will have an empty main diagonal and repeats every one of the 380 tests (=2*190, ncol(combn(length(lus), 2))) comparisons once
tests <- matrix(rep(NA, 20*20), ncol=20,
                dimnames=list(FILES=substr(basename(chat.files), 7, 8),
                              FILES=substr(basename(chat.files), 7, 8)))
for(i in seq(lus)) {    # for each number from 1 to number of files
   for(j in seq(lus)) { # for each number from 1 to number of files
      if(i==j) {        # if a file is compared to itself
         next           # then leave that cell as NA
      } else {          # otherwise
         tests[i,j] <- -log10(   # store in the cell the negative log to the base of 10 of
            wilcox.test(         # the result of a Wilcoxon test comparing
               unlist(lus[[i]]), # the utterance lengths of the current first comparison file and
               unlist(lus[[j]])  # the utterance lengths of the current second comparison file
            )$p.value)           # specifically the p-value
      } # end of conditional expression
   } # end of inner loop j
} # end of outer loop i
round(tests, 2) # inspect the results
# with a simplistic and overly conservative correction for post-hoc tests (Bonferroni),
# all values in that matrix >3.579784 (-log10(0.05/190)) are significant at the 5% level.
# we're ignoring the fact that one might have wanted to do one-tailed tests.

ages <- sapply(sub("^.*_", "", names(mlus)), age.converter)
par(mar=c(5, 4, 4, 5)+0.1) # define the widths of the plotting margins: down, left, top, right
plot(ages, mlus, type="b", pch="m", col="red",         # plot the MLU-values per file against the ages using red m's
     xlim=c(1.25, 2.5), ylim=c(1, 5), axes=FALSE,      # define axis limits and suppress axes
     main="Eve's data from CHILDES", xlab="", ylab="") # generate a main heading and suppress axis labels
   axis(1, at=seq(1.25, 2.5, 0.25))              # create a lower x-axis with tickmarks at seq(1.25, 2.5, 0.25)
      mtext("Recording age", side=1, line=3)     # create a lower x-axis label 3 lines in from the margin
   axis(2, at=1:5, col="red", col.axis="red")    # create a red left y-axis with red labels at 1:5
      mtext("MLUs", side=2, line=2.5, col="red") # create a red left y-axis label 2.5 lines in from the margin
   axis(4, at=1:5, labels=seq(0, 1, 0.25), col="blue", col.axis="blue") # create a blue left y-axis with blue labels of seq(0, 1, 0.25) at 1:5
      mtext("TTRs", side=4, line=2.5, col="blue") # create a blue right y-axis label 2.5 lines in from the margin
   lines(ages, 1+4*(mttrs), type="b", pch="t", col="blue") # plot the TTR-values per file against the ages using blue t's
   # see how the values on the right y-axis - seq(0, 1, 0.25) -
   # map onto those of the left: 1+4*(seq(0, 1, 0.25))
   lines(lowess(mlus ~ ages), col="red", lwd=3)
   lines(lowess((1+4*(mttrs)) ~ ages), col="blue", lwd=3)
   grid()
par(mar=c(5, 4, 4, 2)+0.1) # restore the default plotting margins



# a more specific visual comparison of just the first and last recording
par(mfrow=c(1, 3)) # set up a plotting window with 3 panels (1 rows * 3 columns)
plot(jitter(lus[[1]]), jitter(ttrs[[1]]),                # plot the jittered type-token ratios against the jittered MLUs (file 1)
     pch=16, col=rgb(255, 0, 0, 50, maxColorValue=255),  # with filled grey circles and some transparency effect
     xlab="Mean lengths of utterances", ylab="Type-token ratio per utterance", # these axes labels
     main=names(lus)[[1]],                               # the file name as the main heading
     xlim=c(0, max(unlist(lus))), ylim=c(0, 1)); grid()  # and x- and y-axis limits large enough for all values (and a grid)
   lines(seq(max(unlist(lus))), 1/seq(max(unlist(lus)))) # plot a line that shows where points would be if every utt had only 1 type
plot(jitter(lus[[20]]), jitter(ttrs[[20]]),              # plot the jittered type-token ratios against the jittered MLUs (file 20)
     pch=16, col=rgb(0, 0, 255, 50, maxColorValue=255),  # with filled grey circles and some transparency effect
     xlab="Mean lengths of utterances", ylab="Type-token ratio per utterance", # these axes labels
     main=names(lus)[[20]],                              # the file name as the main heading
     xlim=c(0, max(unlist(lus))), ylim=c(0, 1)); grid()  # and x- and y-axis limits large enough for all values (and a grid)
   lines(seq(max(unlist(lus))), 1/seq(max(unlist(lus)))) # plot a line that shows where points would be if every utt had only 1 type
plot(ecdf(unlist(lus[[1]])), verticals=TRUE, col="red", pch="1", # plot the cumulative percentages of utt lengths of file 1 in red
     xlab="Lengths of utterances", ylab="Cumulative percentage", main="ECDFs of utterance lengths", # with these labels and heading
     xlim=c(0, max(unlist(lus))), ylim=c(0, 1)); grid() # and x- and y-axis limits large enough for all values (and a grid)
   lines(ecdf(unlist(lus[[20]])), verticals=TRUE, col="blue", pch="2") # add the cumulative percentages for file 20 in blue
par(mfrow=c(1, 1)) # restore the default paneling
