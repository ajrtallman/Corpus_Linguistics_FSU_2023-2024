rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the package for rchoose.files
library(dplyr) # load the package dplyr for %>%
# let's assume this is the corpus
(tokens <- c("a", "b", "c", "a", "d", "e", "f", "a", "c", "g"))

# then it's overall type-token ratio is easy:
length(unique(tokens)) / length(tokens)

# but we want to know the number of observed types for _every token slot_ in the data,
# i.e. the result should be
(type.freqs_y.coords <- c(1, 2, 3, 3, 4, 5, 6, 6, 6, 7))

# so that we can plot Figure 35
plot(type.freqs_y.coords ~ seq(type.freqs_y.coords), type="b", pch=" ",
     xlim=c(0, 10), ylim=c(0, 10),
     xlab="Sequence of tokens", ylab="Numbers of types"); grid();
     text(seq(type.freqs_y.coords), type.freqs_y.coords, tokens)
   abline(0, 1, lty=2) # this is the line we'd expect if each word was a new token/type

ttrs <- type.freqs_y.coords <- rep(0, length(tokens)) # generate collector vectors for tts and vocabulary-growth values
for (i in seq(tokens)) { # for each position in the vector tokens
   type.freqs_y.coords[i] <- length(unique(tokens[1:i])) # store the number of types from 1 to the i-th token
   ttrs[i] <- type.freqs_y.coords[i] / i
}
type.freqs_y.coords
round(ttrs, 3)

# thus, we generate a vector of type counts that has as many elements as there are tokens
(type.freqs <- rep(0, length(tokens))) # generate a vector for type counts

# then we determine all unique word types
(types <- unique(tokens))

# determine we determine the first occurrence of each word type in the tokens
(first.occs <- match(types, tokens))

# then we mark every first occurrence of a type with a 1 (that's when the type count must be incremented)
# while leaving all other slots at 0 (that's when the type count need not be incremented)
type.freqs[first.occs] <- 1
type.freqs

# and then we compute the cumulative sums of all these 1s that mark new types
(type.freqs <- cumsum(type.freqs))



# excursus: another way to get first.occs. ...
table(tokens, seq(tokens))
# tokens 1 2 3 4 5 6 7 8 9 10
#      a 1 0 0 1 0 0 0 1 0  0
#      b 0 1 0 0 0 0 0 0 0  0
#      c 0 0 1 0 0 0 0 0 1  0
#      d 0 0 0 0 1 0 0 0 0  0
#      e 0 0 0 0 0 1 0 0 0  0
#      f 0 0 0 0 0 0 1 0 0  0
#      g 0 0 0 0 0 0 0 0 0  1
apply(table(tokens, seq(tokens)), 1, function(qwe) which(qwe==1)[1])
# a  b  c  d  e  f  g
# 1  2  3  5  6  7 10
# with a vector of 500000 randomly-sapled occurrences of the 26 letters from a-z,
# this takes 3 seconds, but our function ttr will take much less.
# Plus, this will become much slower as the type frequency increases ...



# define a function ttr that
# - takes as input a vector of tokens
# - returns as output vectors with type and and token counts, vocabulary-growth values,
#   and maybe already a plot)
ttr <- function (tokens, plot=FALSE, ...) { # define a function ttr (note the ...)
   type.freqs <- rep(0, length(tokens)) # generate a vector for type counts
   types <- unique(tokens)              # determine all unique types
   first.occs <- match(types, tokens)   # determine the first occurrences of each type in the vector of tokens
   type.freqs[first.occs] <- 1          # prepare to increment a cumulative counter by 1 whenever a new type is encountered
   type.freqs <- cumsum(type.freqs)     # compute the (cumulative) type frequencies

   if(plot) { # conditional expression: if the user wants a plot, then
      token.sequence <- seq(tokens)    # generate x-values for the plot
      plot(token.sequence, type.freqs, # plot the cumulative new-type frequencies against the sequence of tokens
           xlab="Sequence of tokens",  # with this x-axis label
           ylab="Numbers of types",    # with this y-axis label
           ...)                        # and use any other arguments provided by the user for plotting
      grid(); abline(0, 1, lty=3)      # add a grid and a line representing the case where every type is used only once
   } # end of conditional expression
   output <- list("Token#"=length(tokens),
                  "Type#"=max(type.freqs),
                  "Tokens in order of occurrence"=tokens[first.occs],
                  "Positions of first occurrences)"=first.occs,
                  "TypesCounts"=type.freqs)
   return(output)
} # end of function definition

par(mfrow=c(2,2))
ttr(tokens, plot=TRUE)
ttr(tokens, plot=TRUE, type="h", col="purple", lwd=8)
ttr(tokens, plot=TRUE, type="b", pch=16)
ttr(tokens, plot=TRUE, type="b", pch=16, ylim=c(0, length(tokens)))
par(mfrow=c(1,1))



# load Hamlet (note: blank.lines.skip=FALSE)
ham <- tolower(scan(rchoose.files(), what=character(), sep="\n", blank.lines.skip=FALSE)) # <corp_hamlet.txt>

# identify lines to be deleted in multiple steps
lines.empty.h <- which(nchar(ham)==0) # identify empty lines
lines.with.names.h <- lines.empty.h+1 # identify lines that typically mention the name of the next speaker

lines.with.stage.directions.h <- grep("^\\[",    # find lines with stage directions (beginning with "[")
                                      ham,       # in Hamlet
                                      perl=TRUE) # using Perl-compatible regular expressions

# compile all lines to be deleted
lines.to.delete.h <- c(lines.empty.h,                 # add to positions of empty lines
                       lines.with.names.h,            # those with speaker names
                       lines.with.stage.directions.h) # and those with stage directions

# delete the lines to be deleted
ham.cleaned <- ham[-lines.to.delete.h]

# split the file up again into words
ham.words <- unlist(strsplit(ham.cleaned, # split up the one long Hamlet string
                             "[^a-z']+",  # at 1+ characters that are not letters or apostrophes
                             perl=TRUE))  # using Perl-compatible regular expressions

# same for Macbeth
mac <- tolower(scan(rchoose.files(), what=character(), sep="\n", blank.lines.skip=FALSE)) # <corp_macbeth.txt>
lines.empty.m <- which(nchar(mac)==0); lines.with.names.m <- lines.empty.m+1 # same for Macbeth
lines.with.stage.directions.m <- grep("^\\[", mac, perl=TRUE) # same for Macbeth
lines.to.delete.m <- c(lines.empty.m, lines.with.names.m, lines.with.stage.directions.m) # same for Macbeth
mac.cleaned <- mac[-lines.to.delete.m] # same for Macbeth
mac.words <- unlist(strsplit(mac.cleaned, "[^a-z']+", perl=TRUE)) # same for Macbeth



# excursus: this is how you could use %>% from the package dplyr to get from the loaded file to ham.words and mac.words
# to.delete.1 <- ham %>%
#    nchar           %>%
#    "=="(0)         %>%
#    which
# to.delete.2 <- to.delete.1 + 1
# to.delete.3 <- ham %>%
#    grep("^\\[", ., perl=TRUE)
# ham.words <-
#    c(to.delete.1, to.delete.2, to.delete.3) %>%
#    "-"(0, .)                                %>%
#    "["(ham, .)                              %>% # ham.cleaned
#    strsplit("[^a-z']+", perl=TRUE)          %>%
#    unlist
# # same for Macbeth
# to.delete.1 <- mac %>% nchar %>% "=="(0) %>% which
# to.delete.2 <- to.delete.1 + 1
# to.delete.3 <- mac %>% grep("^\\[", ., perl=TRUE)
# mac.words <- c(to.delete.1, to.delete.2, to.delete.3) %>%
#    "-"(0, .) %>% "["(mac, .) %>% strsplit("[^a-z']+", perl=TRUE) %>% unlist



ham.ttr <- ttr(ham.words, # apply the function ttr to the words of Hamlet
               plot=TRUE, # and get the plot
               xlim=c(0, 30000), ylim=c(0, 5000), # set the axis limits such that both plays fit
               main="Vocabulary-growth curves for\nHamlet (black) vs. Macbeth (blue)")
   points(seq(mac.words),                   # add the points for all tokens of Macbeth
          (mac.ttr <- ttr(mac.words))[[5]], # and their numbers of types (from part 5 of the output of ttr)
          col="blue")                       # in blue



# try resampling (100 times)
no.of.resamples <- 100
collector <- list(                        # to collect resampling results, create a list with 2 elements
   HAMLET=vector(mode="list",             # one called HAMLET, which is itself a list ...
                 length=no.of.resamples), # ... with with no.of.resamples elements
   MACBETH=vector(mode="list",            # the other called MACBETH, which is itself a list as well ...
                  length=no.of.resamples) # ... and also with with no.of.resamples elements
) # end of list(...)

for(i in seq(no.of.resamples)) { # for each time you want to resample
   collector$HAMLET[[i]] <- # put into the i-th element of the collector's part for Hamlet results
      ttr(                  # the numbers of types of
         sample(ham.words)  # Hamlet with its words randomly reordered
      )[[5]]                # using part 5 of the output of ttr
   collector$MACBETH[[i]] <- ttr(sample(mac.words))[[5]] # same for Macbeth
}

# prepare for plotting: y-axis values (i.e. type frequencies)
all.hamlet.yvalues <- unlist(collector$HAMLET)   # move all numbers of types for Hamlet into one long vector
all.macbeth.yvalues <- unlist(collector$MACBETH) # same for Macbeth

# prepare for plotting: x-axis values (i.e. sequences from 1 to lengths of the plays)
all.hamlet.xvalues <- rep(seq(ham.words), no.of.resamples)  # create x-axis values for all tokens of Hamlet
all.macbeth.xvalues <- rep(seq(mac.words), no.of.resamples) # same for Macbeth

png("../_outputfiles/05_23_Rplot2.png", width=8, height=8, units="in", res=150) # open a graphics device
plot(all.hamlet.xvalues,  # plot at the Hamlet x-coordinates
     all.hamlet.yvalues,  # and the Hamlet y-coordinates
     pch=".", col=rgb(0, 0, 255, 10, maxColorValue=255), # tiny blue dots
     xlab="Sequence of tokens", ylab="Numbers of types", # set axis labels and
     xlim=c(0, 30000), ylim=c(0, 5000),                  # the axis limits
     main="Vocabulary-growth curves for\nHamlet (blue) vs. Macbeth (red)") # set the main heading
   points(all.macbeth.xvalues, all.macbeth.yvalues, pch=".",
          col=rgb(255, 0, 0, 10, maxColorValue=255)) # same for Macbeth, now in red
  grid(); abline(0, 1, lty=2) # add a grid and a line representing the case where every token is used only once
dev.off() # close the graphics device
# quite some overlap ...
