rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.dirs
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

load("../_outputfiles/05_22_adjorder1.RData")

# retain all adjectives that are as what is the frequency of the 5000th most frequent adjective
adj.2.annotate <- sort(names(all.adjectives.freqs[all.adjectives.freqs>=all.adjectives.freqs[5000]]))

# housekeeping
rm(all.adjectives.freqs)



# we now don't need to loop over files of course ...
current.corpus.file.1 <- scan("../_outputfiles/05_22c_BNC-HHVasBROWN.txt", what=character(), sep="\n", quiet=TRUE) # load the converted corpus file

# delete line-initial annotation
current.corpus.file.2 <- gsub("^HHV ", "",           # delete the first 4 characters of each line (i.e., "HHV ")
                              current.corpus.file.1, # in the character vector current.corpus.file.1
                              perl=TRUE)             # using Perl-compatible regular expressions

current.corpus.file.3 <- paste(current.corpus.file.2, # paste the current corpus file together into one long string
                               collapse=" ")          # with spaces separating the elements

for(j in adj.2.annotate) { # search for each adjective
   current.search.expression <- paste("\\b(", j, ")\\b", sep="") # paste together a search expression
   current.corpus.file.3 <- gsub(current.search.expression, # replace the current search expression
                               "<ADJ>\\1",                  # with an adjective tag followed by the original match
                               current.corpus.file.3,       # in current.corpus.file.3
                               ignore.case=TRUE, perl=TRUE) # case-insensitively, using Perl-compatible regular expressions
} # end of loop

current.adj.doubles <- exact.matches.2(
   "<adj>[-a-z]+ <adj>[-a-z]+", # look for two sequences of letters & hyphens tagged as adjectives
   current.corpus.file.3,       # in current.corpus.file
   case.sens=FALSE,             # case-insensitively and without concordance output
   characters.around=100)       # with 100 characters on each side of the match

all.adj.doubles <- paste("05_22c_BNC-HHVasBROWN.txt", # otherwise, paste the file name of the current corpus file
                         current.adj.doubles[[4]],    # in front of the matches with their context
                         sep="\t")                    # separated by tab stops

all.adj.doubles <- paste(seq(all.adj.doubles),                          # paste numbers from 1 to n
                         gsub("<ADJ>", "", all.adj.doubles, perl=TRUE), # in front of all matches but remove tags again
                         sep="\t")                                      # separated by tab stops

cat("CASE\tFILE\tLINE\tPRECEDING\tMATCH\tSUBSEQUENT", # print into a file a table header
   all.adj.doubles,                                   # and all matches in all.adj.doubles
   sep="\n",                                          # separated by line breaks,
   file="../_outputfiles/05_22d_adjorder3_BNC-HHVasBROWN.csv")    # where the file has this name
