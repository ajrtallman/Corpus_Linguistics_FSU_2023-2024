rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs)

# load the data frame with Kilgarriff's frequency data <all.al> from <_qclwr2/_inputfiles/corp_bnc_sgml_freql.txt>
freqs <- read.table(rchoose.files(), header=TRUE, # read in a table with a header
                    sep=" ",                        # where spaces separate columns
                    comment.char="", quote="")      # without comment or quote characters
str(freqs)  # check the import result
head(freqs) # look at the top of the data frame

freqs.adj <- droplevels(      # create a data frame freqs.adj that contains only the used levels of
   freqs[                     # what remains of freq when you choose only those rows
      grepl("aj", freqs$POS), # where grepl finds "aj" in the POS column
      ]                       # end of subsetting
)                             # end of droplevels(...)

rm(freqs) # we remove freqs from the workspace since there's no need to keep that huge data frame in working memory

# retrieve the relevant parts of the data frames
freqs.ic.adj <- droplevels(         # create a data frame freqs.ic.adj that contains only the used levels of
   freqs.adj[                       # what remains of freqs.adj when you choose only those rows
      grepl("ic$", freqs.adj$WORD), # where grepl finds "ic" at the end of the strings in the WORD column
      ]                                # end of subsetting
)                                   # end of droplevels(...)
freqs.ical.adj <- droplevels(         # create a data frame freqs.ic.adj that contains only the used levels of
   freqs.adj[                         # what remains of freqs.adj when you choose only those rows
      grepl("ical$", freqs.adj$WORD), # where grepl finds "ical" at the end of the strings in the WORD column
      ]                               # end of subsetting
)                                     # end of droplevels(...)

str(freqs.ic.adj); head(freqs.ic.adj)     # check ...
str(freqs.ical.adj); head(freqs.ical.adj) # ... the

# since some adjectives (such as "toxic" or "tonic") might be represented with more than one tag,
# we first sum all frequencies for all adjectives across POS tags
ic.adjectives.freqs <- tapply( # create a data structure ic.adjectives.freqs by applying
   freqs.ic.adj$FREQUENCY,     # to the frequencies of all -ic adjectives
   freqs.ic.adj$WORD,          # a grouping by the -ic adjectives
   sum)                        # and then applying the function sum to each group
ical.adjectives.freqs <- tapply( # create a data structure ical.adjectives.freqs by applying
   freqs.ical.adj$FREQUENCY,     # to the frequencies of all -ical adjectives
   freqs.ical.adj$WORD,          # a grouping by the -ical adjectives
   sum)                          # and then applying the function sum to each group

# now we need to merge the two tables such that for every adjective,
# we have its -ic and its -ical suffixation frequency: we use a logic
# very similar to the one on comparing frequency lists of corpora in exercise box 3.1

ic.adjectives <- rep(names(ic.adjectives.freqs),     # create a vector ic.adjectives by repeating every -ic adjective
                     ic.adjectives.freqs)            # as often as it is attested in the corpus
ical.adjectives <- rep(names(ical.adjectives.freqs), # create a vector ical.adjectives by repeating every -ical adjective
                       ical.adjectives.freqs)        # as often as it is attested in the corpus

both.adjectives <- c(ic.adjectives,      # create a vector both.adjectives by combining the -ic adjective tokens
                     gsub("ical$", "ic", # with what you get when you replace -ical with -ic suffixes
                          ical.adjectives, # in the -ical adjective tokens
                          perl=TRUE))    # using Perl-compatible regular expressions
their.suffixes <- rep(c("ic", "ical"),   # create a vector their.suffixes by repeating each suffix
                      c(length(ic.adjectives), length(ical.adjectives))) # as often as it was attested

result <- table(both.adjectives, their.suffixes) # cross-tabule the -ic adjective forms with the suffix they were used with
result.perc <- round(prop.table(result, 1), 4)   # change this into a table of row %s, rounded to 4 digits
order.index <- order(rowSums(result), decreasing=TRUE) # create an order.index on the basis of the adjectives' joint freq

head(result <- result[order.index,], 20) # reorder result accordingly and look at its first 20 rows
head(result.perc <- result.perc[order.index,], 20) # reorder result.perc accordingly and look at its first 20 rows



# check for all adjective pairs whether there is a correlation between
# - the logged frequency of the adjective stem and
# - the % of -ical forms
plot(log(rowSums(result)), # plot on the x-axis the logged frequencies of the adjective stems
     result.perc[,2],      # plot on the y-axis the % of -ical adjectives
     xlab="Frequency of -ic/-ical adjective pair", # with this x-axis label
     ylab="% of -ical forms / adjective pair",     # with this y-axis label
     pch=16, col=rgb(0, 0, 0, 30, maxColorValue=255)) # full light grey circles with alpha transparency of 30
   grid() # add a grid
   lines(lowess(result.perc[,2] ~         # add a smoother to summarize of the -ical %s behave as a function of
                   log(rowSums(result))), # the logged frequencies of the adjective stems
         lwd=3, col="red")                # with a thick red line

# compare the ic-frequencies to the -ical frequencies with a boxplot (see SFLWR2: Section 3.2.2.1)
boxplot(result[,1]+1,          # plot the -ic frequencies
        result[,2]+1,          # against the -ical frequencies
        notch=TRUE, log="y",   # with notches and a logged y-axis
        names=c("ic", "ical")) # and these x-axis tickmarks
# compute a Wilcoxon test the check whether the -ic frequencies are significantly different from the ical-frequencies (see SFLWR2: Section 4.3.2.4)
wilcox.test(result[,1],  # compare the -ic-frequencies
            result[,2],  # to the -ic-frequencies
            paired=TRUE) # in a pairwise test (pairwise across stems)

# given the not-so-usefulness of the boxplots, the higher resolution of an ecdf plot might be more instructive:
plot(ecdf(log(result[,1]+1)),             # plot an ecdf curve of the -ic frequencies
     verticals=TRUE, col="blue", main="", # with blue vertical lines and points and no heading
     xlim=c(0, 11), ylim=c(0, 1),         # with these axes limits (to make them the same as for the plot below)
     xlab="Logged frequency", ylab="Empirical cumulative distribution") # and these axes labels
   lines(ecdf(log(result[,2]+1)),   # add a corresponding line for the -ical frequencies
         verticals=TRUE, col="red") # with red vertical lines and points
   legend(7, 0.3,                 # add a legend at these x- and y-coordinates
          fill=c("red", "blue"),  # using red and blue filled squares
          legend=c("ic", "ical"), # for ic and ical
          bty="n", ncol=2,        # no box around the legend, which has two columns
          xjust=0.5, yjust=0.5)   # and is centered around the above coordinates
   grid() # add a grid
ks.test(result[,1], # compare the difference between the ecdf curve of the -ic frequencies
        result[,2]) # against that of the -cal frequencies
# highly significant! There's a warning because of the ties, which one could avoid by jittering



# check for the adjective pairs that are attested with both suffixes a user-defined minimum number of times whether there is a correlation between
# - the logged frequency of the adjective stem and
# - the % of ical-forms
threshold.to.be.met <- 3 # set the minimum required frequency to 3
result.trimmed <- result[  # reduce result to a version of result with only those rows
   apply(result, 1, min)>= # whose minimum value is greater than or equal to
      threshold.to.be.met, # the minimum required frequency
   ] # end of subsetting
result.perc.trimmed <- result.perc[apply(result, 1, min)>=threshold.to.be.met,] # same for result.perc

head(result.trimmed)      # check ...
head(result.perc.trimmed) # ... the results

plot(log(rowSums(result.trimmed)), # plot on the x-axis the logged frequencies of the adjective stems
     result.perc.trimmed[,2],      # plot on the y-axis the % of -ical adjectives
     xlab="Frequency of -ic/-ical adjective pair", # with this x-axis label
     ylab="% of -ical forms / adjective pair",     # with this y-axis label
     pch=16, col=rgb(0, 0, 0, 50, maxColorValue=255)) # full light grey circles with alpha transparency of 30
   grid() # add a grid
   lines(lowess(result.perc.trimmed[,2] ~         # add a smoother to summarize of the -ical %s behave as a function of
                   log(rowSums(result.trimmed))), # the logged frequencies of the adjective stems
         lwd=3, col="red")                        # with a thick red line

# compare the ic-frequencies to the -ical frequencies with a boxplot (see SFLWR2: Section 3.2.2.1)
boxplot(result.trimmed[,1],    # plot the -ic frequencies
        result.trimmed[,2],    # against the -ical frequencies
        notch=TRUE, log="y",   # with notches and a logged y-axis
        names=c("ic", "ical")) # and these x-axis tickmarks
# compute a Wilcoxon test to check whether the ic-frequencies are significantly different from the ical-frequencies (see SFLWR2: Section 4.3.2.4)
wilcox.test(result.trimmed[,1], # compare the -ic-frequencies
            result.trimmed[,2], # to the -ical-frequencies
            paired=TRUE)        # in a pairwise test (pairwise across stems)


# given the not-so-usefulness of the boxplots, the higher resolution of an ecdf plot might be more instructive:
plot(ecdf(log(result.trimmed[,1]+1)),     # plot an ecdf curve of the -ic frequencies
     verticals=TRUE, col="blue", main="", # with blue vertical lines and points and no heading
     xlim=c(0, 11), ylim=c(0, 1),         # with these axes limits (to make them the same as for the plot above)
     xlab="Logged frequency", ylab="Empirical cumulative distribution") # and these axes labels
   lines(ecdf(log(result.trimmed[,2]+1)), # add a corresponding line for the -ical frequencies
         verticals=TRUE, col="red")       # with red vertical lines and points
   legend(7, 0.3,                 # add a legend at these x- and y-coordinates
          fill=c("red", "blue"),  # using red and blue filled squares
          legend=c("ic", "ical"), # for ic and ical
          bty="n", ncol=2,        # no box around the legend, which has two columns
          xjust=0.5, yjust=0.5)   # and is centered around the above coordinates
   grid() # add a grid
ks.test(result.trimmed[,1], # compare the difference between the ecdf curve of the -ic frequencies
        result.trimmed[,2]) # against that of the -cal frequencies
# not significant anymore! There's a warning because of the ties, which one could avoid by jittering
