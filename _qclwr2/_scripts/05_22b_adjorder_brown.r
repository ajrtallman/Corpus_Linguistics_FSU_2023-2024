rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.dirs
library(XML)   # load the package XML for XML processing
library(xml2)  # load the package xml2 for XML processing
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

# choose the directory where BNC XML corpus files are located
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# prepare the long collector vector and its first position to be filled
all.adjectives <- character(10000000)
counter <- 1

for(i in corpus.files) { # for the i-th file (i iterating over all selected corpus files)
   cat(basename(i), "\n") # output a progress report to the screen

   if(.Platform$OS.type=="windows") { # if you are using this on a Windoze system
      # use the package xml2
      current.corpus.file <- read_xml(i) # import a corpus file and parse its XML tree
      current.adjectives <- tolower( # put into current.adjectives the result of tolower-ing
         as.character(               # the character vector you get from
            xml_find_all(            # finding all XML elements
               current.corpus.file,  # in current.corpus.file
               ".//w[@c5 = 'AJ0']")) # that are words with the c%-tag AJ0
      ) # end of tolower(...)
      current.adjectives <- gsub( # remove
      "<.*?>", "",                # all tags
      current.adjectives,         # from current.adjectives
      perl=TRUE)                  # using Perl-compatible regular expressions
   } else {                           # if you are using this on another operating system
      # use the package XML
      current.corpus.file <- xmlInternalTreeParse(i) # import a corpus file and parse its XML tree
      current.adjectives <- tolower(      # the result of switching to lower case
         xpathSApply(current.corpus.file, # apply to the XML structure of current.corpus.file
                     "//w[@c5 = 'AJ0']",  # namely the things tagged as adjectives
                     xmlValue)            # and extract the value (i.e., the adjective)
      ) # end of tolower(...)
   } # end of conditional expression

   current.adjectives <- grep("^[-a-z]+$", # all retrieved adjectives consisting only of letters and hyphens
           whitespace(current.adjectives), # when whitespace has been stripped of all elements of current.adjectives
           perl=TRUE,                      # using Perl-compatible regular expressions
           value=TRUE)                     # retain the values, not the position indices

   if(length(current.adjectives)==0) { next } # if there were no adjectives in that file, iterate (i.e., jump to the next file)
   all.adjectives[counter: # put into the elements of all.adjectives that begin at the current value of counter
                     (counter-1+ # and end at the position when one has
                         length(current.adjectives))] <- # added the current adjectives
      current.adjectives # the current adjectives
   counter <- counter+length(current.adjectives) # define the new starting position of counter for the next batch of adjectives
} # end of loop

all.adjectives <- all.adjectives[nzchar(all.adjectives)]             # get rid of the empty elements of the character vector
all.adjectives.freqs <- sort(table(all.adjectives), decreasing=TRUE) # create a sorted frequency table of all adjectives and ...
save(all.adjectives.freqs, file="../_outputfiles/05_22_adjorder1.RData")                # ... save them into an RData file

# retain all adjectives that are as what is the frequency of the 5000th most frequent adjective
adj.2.annotate <- sort(names(all.adjectives.freqs[all.adjectives.freqs>=all.adjectives.freqs[5000]]))

# housekeeping
rm(all.adjectives, all.adjectives.freqs)



# choose a folder with the Brown corpus (I am using the files in BROWN1 from the ICAME2 CD)
corpus.files <- rchoose.files()

# prepare collector
all.adj.doubles <- character()

for(i in corpus.files) { # for the i-th file (i iterating over all selected corpus files)
   cat(basename(i), "\n") # output a progress report to the screen

   current.corpus.file.1 <- scan(i, what=character(), sep="\n", quiet=TRUE) # load the i-th/current corpus file
   # delete line-initial annotation
   current.corpus.file.2 <- gsub("^.........", "",      # delete the first 9 characters of each line (e.g., "A01 0010 ")
                                 current.corpus.file.1, # in the character vector current.corpus.file.1
                                 perl=TRUE)             # using Perl-compatible regular expressions

   current.corpus.file.3 <- paste(current.corpus.file.2, # paste the current corpus file together into one long string
                                  collapse=" ")          # with spaces between the original corpus lines

   current.corpus.file.4 <- whitespace(unlist( # unlist the result of
      strsplit(current.corpus.file.3,          # split up the one long character vector with the whole corpus file
               "   ")                          # at every occurrence of 3 spaces that we often see mark sentence boundaries
   )) # end of whitespace(unlist(...))

   current.corpus.file.5 <- current.corpus.file.4[nzchar(current.corpus.file.4)]

   for(j in adj.2.annotate) { # search for each adjective
      current.search.expression <- paste("\\b(", j, ")\\b", sep="") # paste together a search expression
      current.corpus.file.5 <- gsub(current.search.expression,      # replace the current search expression
                                  "<ADJ>\\1",                       # with an adjective tag followed by the original match
                                  current.corpus.file.5,            # in current.corpus.file.5
                                  perl=TRUE)                        # using Perl-compatible regular expressions
   } # end of inner loop

   current.adj.doubles <- exact.matches.2(
      "<adj>[-a-z]+ <adj>[-a-z]+", # look for two sequences of letters & hyphens tagged as adjectives
      current.corpus.file.5,       # in current.corpus.file
      case.sens=FALSE)             # case-insensitively

   if(is.null(current.adj.doubles[[1]])) { next } # if there were no adjectives in that file, iterate
   current.adj.doubles <- paste(basename(i),      # otherwise, paste the file name of the current corpus file
                                current.adj.doubles[[2]], # in front of the sentence numbers of the matches
                                current.adj.doubles[[4]], # in front of the matches with their context
                                sep="\t")                 # separated by tab stops
   all.adj.doubles <- c(all.adj.doubles,     # create a new version of all.adj.doubles by adding
                        current.adj.doubles) # current.adj.doubles

} # end of outer loop

all.adj.doubles <- paste(seq(all.adj.doubles),                          # paste numbers from 1 to n
                         gsub("<ADJ>", "", all.adj.doubles, perl=TRUE), # in front of all matches but remove tags again (for readability)
                         sep="\t")                                      # separated by tab stops

cat("CASE\tFILE\tLINE\tPRECEDING\tMATCH\tSUBSEQUENT", # print into a file a table header
   all.adj.doubles,                                   # and all matches in all.adj.doubles
   sep="\n",                                          # separated by line breaks,
   file="../_outputfiles/05_22b_adjorder3_brown.csv") # where the file has this name
