rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for tk_choose_dir
source(file.choose()) # source the function exact.matches.2

# define the function whitespace
whitespace <- function (input.vector, leadtrail=TRUE, reduce=TRUE, empties=TRUE) {
   if(reduce) { # if reduce is TRUE
      input.vector <- gsub(" {2,}", " ", input.vector, perl=TRUE) # replace all occ of 2+ spaces by 1 space
   }
   if(leadtrail) { # if leadtrail is TRUE
      input.vector <- gsub("(^ +| +$|)", "", input.vector, perl=TRUE) # replace leading/trailing spaces by nothing
   }
   if(!empties) { # if empties is FALSE
      input.vector <- input.vector[nzchar(input.vector)]
   }
   return(input.vector)
}

# define the directory where the BNC files folder A are located and get the file names
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# define search expressions
search.expression.split    <- "<w c5=\"to0\"[^>]+>to <w c5=\"av0\" hw=[^>]+>[^<]+<w c5=\"v..(-...)?\" hw=[^>]+>[^<]+"
search.expression.nonsplit <- "<w c5=\"to0\"[^>]+>to <w c5=\"v..(-...)?\" hw=[^>]+>[^<]+<w c5=\"av0\" hw=[^>]+>[^<]+"

# prepare collectors and results vectors
all.splits <- all.nonsplits <- character()
dir.create("../_outputfiles/05_17_freqoutput") # create a subdirectory for the frequency list files

for(i in corpus.files) { # access each file
   cat( # output a progress report to the screen
      basename(i), "\n"
      ) # the file that will be loaded on this iteration, followed by a line break

   current.corpus.file <- tolower(scan(i, what=character(), sep="\n", quiet=TRUE)) # load the i-th/current corpus file and switch it to lower case

   # retrieve only the lines with sentences in them (i.e., discard the header, utterance tags, etc.)
   current.sentences.1 <- grep("<s n=",             # find the lines that have sentence number tags in them
                               current.corpus.file, # in the i-th/current corpus file
                               perl=TRUE,           # using Perl-compatible regular expressions
                               value=TRUE)          # and return the (whole line) match, not its position index

   # get rid of everything that's not a word and its annotation
   current.sentences.2 <- gsub("<(?!w)[^<]*", # find all occurrences of stuff that's not tagged as a word and its tag
                               "",            # replace them by nothing
                               current.sentences.1, # in the vector current.sentences.1
                               perl=TRUE)           # using Perl-compatible regular expressions

   # find and store the cases of split and non-split infinitives
   all.splits <- c(all.splits, # make the new content of all.splits its old content plus
                   exact.matches.2(search.expression.split,     # the result of the search for split infinitives
                                   current.sentences.2,         # in current.sentences.2
                                   gen.conc.output=FALSE)[[1]]) # suppress concordance output, only get the exact matches
   all.nonsplits <- c(all.nonsplits, # make the new content of all.nonsplits its old content plus
                      exact.matches.2(search.expression.nonsplit,  # the result of the search for non-split infinitives
                                      current.sentences.2,         # in current.sentences.2
                                      gen.conc.output=FALSE)[[1]]) # suppress concordance output, only get the exact matches

   # find and count all lemmas
   current.lemmas <- exact.matches.2("(?<=hw=\")[^\"]+",         # find what's after hw=" that's not a "
                                     current.sentences.2,        # in current sentences.2
                                     gen.conc.output=FALSE)[[1]] # suppress concordance output, only get the exact matches
   current.lemma.freqs <- table(current.lemmas) # generate a frequency list of all lemmas of the i-th/current corpus file
   save(current.lemma.freqs,          # save that frequency list into
        file=paste(                   # a file ...
           "../_outputfiles/05_17_freqoutput/", # whose path begins with the directory for these lists ...
           substr(basename(i), 1, 3), # whose name is the 3-character identifier of the BNC file name
           ".RData", sep=""))         # whose extension is .RData

} # end of loop

all.splits.split <- strsplit(all.splits, " *<.*?>", perl=TRUE) # split up the split matches at the tags into a list
   splits.adverbs <- whitespace(sapply(all.splits.split, "[", 3)) # extract the adverbs from the 3rd slot of that list
   splits.verbs   <- whitespace(sapply(all.splits.split, "[", 4)) # extract the verbs from the 4th slot of that list
all.nonsplits.split <- strsplit(all.nonsplits, " *<.*?>", perl=TRUE) # split up the nonsplit matches at the tags into a list
   nonsplits.adverbs <- whitespace(sapply(all.nonsplits.split, "[", 4)) # extract the adverbs from the 4th slot of that list
   nonsplits.verbs   <- whitespace(sapply(all.nonsplits.split, "[", 3)) # extract the verbs from the 3rd slot of that list

length(all.splits) # how many split infinitives are there?
   head(sort(table(splits.adverbs), decreasing=TRUE), 20) # the 20 most frequent adverbs in split infinitives
   head(sort(table(splits.verbs), decreasing=TRUE), 20)   # the 20 most frequent verbs in split infinitives
length(all.nonsplits) # how many non-split infinitives are there?
   head(sort(table(nonsplits.adverbs), decreasing=TRUE), 20) # the 20 most frequent adverbs in non-split infinitives
   head(sort(table(nonsplits.verbs), decreasing=TRUE), 20)   # the 20 most frequent adverbs in non-split infinitives



# let's compute the percentages for only those adverbs that are attested in both infinitives
shared.adverbs <- intersect(        # define shared.adverbs as the intersection of
   names(table(nonsplits.adverbs)), # the adverbs that occur with non-split infinitives
   names(table(splits.adverbs))     # the adverbs that occur with split infinitives
)

result.adv <- matrix( # define a matrix called result.adv that contains
   c(table(nonsplits.adverbs)[shared.adverbs], # the frequencies of shared adverbs in non-split infinitives
     table(splits.adverbs)[shared.adverbs]),   # the frequencies of shared adverbs in split infinitives
   ncol=2,                                     # in two columns
   dimnames=list(SHAREDADVERBS=shared.adverbs, # with the shared adverbs as row names
                 SPLIT=c("no", "yes"))         # and SPLIT no vs. yes as column names
); result.adv                                  # end of matrix(...) and look at it

# here's an alternative way to create result.adv:
# adverbs.all <- c(splits.adverbs, nonsplits.adverbs)
# split.all <- rep(c("yes", "no"), c(length(splits.adverbs), length(nonsplits.adverbs)))
# result.adv <- table(adverbs.all, split.all)
#    result.adv <- result.adv[apply(result.adv, 1, function(x) min(x)>0),]

# let's compute how the %s of splits per adverb compare the overall % of splits
result.adv.perc.split <- prop.table(result.adv, 1)[,2] # store the percentages of splits per adverb
adv.pref.4.split <- result.adv.perc.split -            # subtract from these percentages
   (length(all.splits) /                               # the overall percentage ...
       length(c(all.splits, all.nonsplits)))           # ... of split constructions in the data
summary(adv.pref.4.split <- sort(adv.pref.4.split))    # sort them by differences and look at a summary of them

plot(adv.pref.4.split, type="n", ylim=c(-0.1, .8), # (don't) plot the %-differences per adverb in a certain coordinate system
     xlab="Sequence of adverbs attested with both split and non-split infinitives",    # with this x-axis label
     ylab="% preference of adverbs to occur in split form compared to non-split form") # with this y-axis label
   grid(); abline(h=0, lty=2) # add a grid and a dashed horizontal line at y=0 (where the adverb has no prefererence)
   text(seq(adv.pref.4.split),   # plot text at x-coordinates 1, ..., n for all adverbs
        adv.pref.4.split,        # plot text at the y-coordinates of the preference differences
        names(adv.pref.4.split), # namely the shared adverbs
        srt=90,                  # rotate the adverbs by 90 degrees
        cex=2/3)                 # reduce their size by 1/3
# or maybe just do dotchart(adv.pref.4.split); abline(v=0, lty=2)



# let's compute the percentages for only those verbs that are attested in split infinitives
shared.verbs <- intersect( # define shared.verbs as the intersection of
   names(table(nonsplits.verbs)), # the verbs that occur with non-split infinitives
   names(table(splits.verbs))     # the verbs that occur with split infinitives
)

result.v <- matrix( # define a matrix called result.adv that contains
   c(table(nonsplits.verbs)[shared.verbs],  # the frequencies of shared verbs in non-split infinitives
     table(splits.verbs)[shared.verbs]),    # the frequencies of shared verbs in split infinitives
   ncol=2,                                  # in two columns
   dimnames=list(SHAREDVERBS=shared.verbs,  # with the shared verbs as row names
                 SPLIT=c("no", "yes"))      # and SPLIT no vs. yes as column names
); result.v                                 # and look at it

# here's an alternative way to create result.v:
# verbs.all <- c(splits.verbs, nonsplits.verbs)
# split.all <- rep(c("yes", "no"), c(length(splits.adverbs), length(nonsplits.adverbs))) # from above
# result.v <- table(verbs.all, split.all)
#     result.v <- result.v[apply(result.v, 1, function(x) min(x)>0),]

# let's compute how the %s of splits per verb compare the overall % of splits
result.v.perc.split <- prop.table(result.v, 1)[,2] # store the percentages of splits per verb
v.pref.4.split <- result.v.perc.split -            # subtract from these percentages
   (length(all.splits) /                           # the overall percentage ...
       length(c(all.splits, all.nonsplits)))       # ... of split constructions in the data
summary(v.pref.4.split <- sort(v.pref.4.split))    # sort them by differences and look at a summary of them

plot(v.pref.4.split, type="n", ylim=c(-0.1, .8), # (don't) plot the %-differences per adverb in a certain coordinate system
     xlab="Sequence of verbs attested with both split and non-split infinitives",    # with this x-axis label
     ylab="% preference of verbs to occur in split form compared to non-split form") # with this y-axis label
   grid(); abline(h=0, lty=2) # add a grid and a dashed horizontal line at y=0 (where the verb has no prefererence)
   text(seq(v.pref.4.split),   # plot text at x-coordinates 1, ..., n for all verbs
        v.pref.4.split,        # plot text at the y-coordinates of the preference differences
        names(v.pref.4.split), # namely the shared verbs
        srt=90,                # rotate the verbs by 90 degrees
        cex=1/2)               # reduce their size to 50%
# or maybe just do dotchart(v.pref.4.split); abline(v=0, lty=2)



# let's explore frequencies of all combinations of verbs and adverbs in split infinitives
coocc.table <- table(splits.verbs, splits.adverbs) # cross-tabulate all verbs and adverbs from split infinitives
coocc.table <- coocc.table[                      # reorder rows and columns of that table by
   order(rowSums(coocc.table), decreasing=TRUE), # its row sums
   order(colSums(coocc.table), decreasing=TRUE)] # its column sums
# now the most frequent verbs of split infinitives are at the top
# now the most frequent adverbs of split infinitives are on the left
coocc.table[1:20,1:20] # but this doesn't help much so ...

# let's explore frequencies of all attested combinations of verbs and adverbs in split infinitives
sort(table(paste(splits.adverbs, splits.verbs, sep=" ")))



setwd("../_outputfiles/05_17_freqoutput") # move to the directory with all the file-specific frequency tables
# define an empty frequency table in which to collect all file-specific frequency tables
all.lemma.freqs <- table(NA)

for(i in dir()) {
   cat( # output a progress report to the screen
      basename(i), "\n"
   ) # the file that will be loaded on this iteration, followed by a line break
   load(i) # load the lemma frequency list from the i-th/current file
   all.lemma.freqs <-      # make the new content of all.lemma.freqs
      c(all.lemma.freqs,   # the old content of all.lemma.freqs
        current.lemma.freqs) # plus the lemma frequencies of the i-th/current file
}

all.lemma.freqs <- tapply( # the result of applying
   all.lemma.freqs,        # to all.lemma.freqs
   names(all.lemma.freqs), # a grouping by the names of all.lemma.freqs
         sum)              # the function sum
   class(all.lemma.freqs) <- "table"   # make sure the data type is a table again
all.lemma.freqs <- sort(all.lemma.freqs, decreasing=TRUE) # sort the resulting table by frequency in descending order

# output lemmas whose observed frequencies do not differ by more that 5% from the frequency of the split construction
sort(                                         # output a sorted version of
   all.lemma.freqs[                           # the overall frequency list, but only those words whose frequencies
      abs(all.lemma.freqs-length(all.splits)) # differ from the frequency ...
      < (length(all.splits)*0.05)             # of split infinitives by less than 5%
      ]
   ) # end of sort (...)

# output lemmas whose observed frequencies do not differ by more that 5% from the frequency of the non-split construction
sort(                                            # output a sorted version of
   all.lemma.freqs[                              # the overall frequency list, but only those words whose frequencies
      abs(all.lemma.freqs-length(all.nonsplits)) # differ from the frequency ...
      < (length(all.nonsplits)*0.05)             # of non-split infinitives by less than 5%
      ]
   ) # end of sort (...)
