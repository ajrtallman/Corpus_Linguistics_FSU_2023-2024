rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.files

log.0 <- function (some.number, base=exp(1)) { # define a function to compute a log of some number with some base (default: natural log)
   ifelse(some.number==0,              # if the number of which the log is computed is 0,
          0,                           # then return 0,
          log(some.number, base=base)) # otherwise return the log of the non-zero number to the user-defined or default base
}

# define the paths to the two corpora to compare
text.files <- rchoose.files() # <corp_perl.txt> and <corp_python.txt>

all.word.tokens <- all.sources <- vector() # generate vectors to store the results

for(i in text.files) { # access each file
   text.file <- tolower(scan(i, what=character(), sep="\n", quiet=TRUE)) # load the i-th/current corpus file

   # split up into words (simplistically)
   curr.word.tokens <- unlist( # create a vector word.tokens by unlisting
      strsplit(text.file,      # the result of splitting up the text file
               "\\W+",         # at occurrences of 1+ non-word characters
               perl=TRUE))     # using Perl-compatible regular expressions
   curr.word.tokens <- curr.word.tokens[nzchar(curr.word.tokens)] # remove empty character strings
   curr.word.tokens[          # take the current word tokens
      grepl("^\\d+$",         # where you find only of 1 or more digits
            curr.word.tokens, # in current word tokens
            perl=TRUE)] <-    # using Perl-compatible regular expressions and change them to
      "_NUM_"                 # "_NUM_"
   all.word.tokens <- c(all.word.tokens,  # create a new version of all.word.tokens by adding to the old version
                        curr.word.tokens) # the current word tokens

   all.sources <- c(all.sources, rep( # add to all corpora the repetitions of
      basename(i),                    # the name of the current corpus file
      length(curr.word.tokens)        # as often as that corpus has words
   ))
} # end of loop



word.by.corp.table <- table(all.word.tokens, all.sources) # cross-tabulate the words and the corpora
head(word.by.corp.table, 20)                              # and check the first 20 rows of that table

# generating vectors for the typical computation of keywords statistics from 2x2 tables
obs.as <- word.by.corp.table[,1] # the individual observed frequencies of each words in the first corpus
obs.bs <- word.by.corp.table[,2] # the individual observed frequencies of each words in the second corpus
obs.cs <- sum(obs.as) - obs.as   # the combined observed frequencies of all other words in the first corpus
obs.ds <- sum(obs.bs) - obs.bs   # the combined observed frequencies of all other words in the second corpus

exp.as <- (obs.as+obs.bs)*(obs.as+obs.cs)/length(all.word.tokens) # the frequency expected for obs.as if the null hypothesis was true
exp.bs <- (obs.as+obs.bs)*(obs.bs+obs.ds)/length(all.word.tokens) # the frequency expected for obs.bs if the null hypothesis was true
exp.cs <- (obs.as+obs.cs)*(obs.cs+obs.ds)/length(all.word.tokens) # the frequency expected for obs.cs if the null hypothesis was true
exp.ds <- (obs.bs+obs.ds)*(obs.cs+obs.ds)/length(all.word.tokens) # the frequency expected for obs.ds if the null hypothesis was true

# computing log-likelihood ratios for all words
llrs <- 2*(
   obs.as*log.0(obs.as/exp.as)+
   obs.bs*log.0(obs.bs/exp.bs)+
   obs.cs*log.0(obs.cs/exp.cs)+
   obs.ds*log.0(obs.ds/exp.ds)
)

# computing difference coefficients for all words
dcs <- (obs.as-obs.bs)/rowSums(word.by.corp.table)

# computing relative frequency ratios for all words
rfrs <- (obs.as/colSums(word.by.corp.table)[1]) / (obs.bs/colSums(word.by.corp.table)[2])

# compile results into one data frame for easy output
results <- data.frame(
   WORD=rownames(word.by.corp.table),
   OBSFREQIN1=obs.as, EXPFREQIN1=exp.as,
   OBSFREQIN2=obs.bs, EXPFREQIN2=exp.bs,
   C=obs.cs, D=obs.ds,
   LOGLIKRATIOS=llrs, DIFFCOEFFS=dcs, RELFREQRATIOS=rfrs,
   PREFERRED=ifelse(obs.as>obs.bs, "corpus1", "corpus2"),
   row.names=NULL
)
results <- results[order(results$PREFERRED,     # re-order the rows of the data frame result by which corpus is preferred by each word,
                         results$DIFFCOEFFS, # , then by the difference coefficients,
                         decreasing=TRUE),]     # and all that in descending order
write.table(results, "../_outputfiles/05_08_keywords.csv",      # save the data frame into a file
            sep="\t", eol="\n",           # tab-separated and line breaks at the end of rows
            row.names=FALSE, quote=FALSE) # no row names and don't put quotes around strings



# elementary plot: plot the words' coefficients of difference against their observed frequency in both corpora
plot(rowSums(word.by.corp.table), dcs)      # plot the combined word frequencies on the x-axis and difference coefficients on the y-axis
   rug(rowSums(word.by.corp.table), side=1) # add rugs on the (bottom) x-axis
   rug(dcs, side=2)                         # add rugs on the (left) y-axis

plot(log(rowSums(word.by.corp.table)), # plot the combined word frequencies on the x-axis (note they are logged) and
     dcs, type="n", axes=FALSE,        # plot the difference coefficients on the y-axis, but don't plot anything and no axes
     xlab="Frequency in both corpora", ylab="Difference coefficients",                        # with these axis labels
     main="Key words comparing the Wikipedia entries for 'perl' (top) and 'python' (bottom)", # and this main heading
     sub="(blue font = significant LLR)")                                                     # and this sub-heading
   text(log(rowSums(word.by.corp.table)), # plot at the same x- and
        dcs,                              # y-axis coordinates
        rownames(word.by.corp.table),     # the words
        cex=0.75, col=ifelse(llrs>=3.841, "blue", "black")) # 25% smaller and in blue when the LLR is significant
   axis(1, at=0:6, labels=round(exp(0:6), 1)); axis(2, at=seq(-1, 1, 0.5)) # add x- and y-axis with labels (antilogged for the x-axis!)
box(); grid() # add a box and a grid
