rm(list=ls(all=TRUE)) # clear memory

# read in the file and split up into words
text.file <- tolower(scan(file.choose(), what=character(), sep="\n", quiet=TRUE)) # <_qclwr2/_inputfiles/corp_perl.txt>

# split up into words (simplistically) and tabulate
word.tokens <- unlist(  # create a vector word.tokens by unlisting
   strsplit(text.file,  # the result of splitting up the text file
            "\\W+",     # at occurrences of 1+ non-word characters
            perl=TRUE)) # using Perl-compatible regular expressions
word.tokens <- word.tokens[nzchar(word.tokens)] # remove empty character strings

# check for every word token whether it is "perl" or not (this will be a vector of TRUEs and FALSEs)
is.perl <- word.tokens=="perl"
# note, this could be written like this, too:
# is.perl <- "=="(word.tokens, "perl")

plot(is.perl,              # plot is.perl (TRUEs and FALSEs, i.e. 1s and 0s)
     type="h", axes=FALSE, # type="h" makes R plot vertical lines from 0 to the values, axis=FALSE suppress all axes
     xlab="", ylab="", main="The distribution of >perl< in the corpus") # no axis labels and this main heading

# identify the desired number of groups
number.of.parts <- 10 # you can choose a different number here

# Next, we need to split up all positions in the corpus into number.of.groups groups. To that end, we use the function cut to first group every position in the corpus into one of the number.of.groups groups and then generate a frequency table of this so that we know how many elements each of the number.of.groups groups has.
# split up the corpus into the desired number of groups
corpus.parts <- cut(seq(word.tokens), breaks=number.of.parts)

# the following table now gives you for each of the corpus parts
# the number of words that do not match the above regex (first column) and
# the number of words that do (second column)
# and the corpus parts (as defined by word numbers) as row names
distribution <- table(corpus.parts, is.perl)
addmargins(distribution)

# If you are also trying to include a measure of dispersion into your script, now you have all the relevant information:
obs.percs <- distribution[,2]/sum(distribution[,2])
exp.percs <- rowSums(distribution)/sum(rowSums(distribution))
DP <- sum(abs(obs.percs-exp.percs))/2

# Finally, we can now create a bar plot. In this bar plot we use percentages as names for the x-axis, which we round those to two decimals to avoid unduly long labels (cf. the names argument), which is no problem with 10 groups, but would be one with 9, 12, etc. Also, we use the percentages (hence prop.table), but only those of the second column (hence "[,2]"), because only those contain the percentages of the matches.
barplot(obs.percs, # generate a bar plot of the percentages of the %s of "perl" in the corpus parts
   xlab="Sections of the corpus", ylab="Relative frequencies of 'perl'", # with these x- and y-axis labels
   space=0,        # no spaces between bars
   ylim=c(0, 0.5), # y-axis limits from 0 to 0.5
   names=round(seq(number.of.parts)/number.of.parts, 2), # x-axis bar labels representing the corpus part percentages
   main="The distribution of >perl< in the corpus",      # this main heading and
   sub=paste0("(Gries's DP =", round(DP, 4), ")") # this sub title under the x-axis label
)
abline(h=1/number.of.parts, lty=3) # add a line representing a perfectly even distribution of "perl"

# if you want to have vertical x-axis labels, add the following uncommented argument to the barplot function
# las=2
