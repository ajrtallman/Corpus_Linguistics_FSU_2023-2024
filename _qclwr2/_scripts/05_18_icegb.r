rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.dir
library(dplyr) # load the package dplyr for %>%
source(file.choose()) # source the function exact.matches.2
options(warn=1) # make R show any warnings as soon as they arise
# Note: Unfortunately, it seems the corpus compilers of the ICE-GB do not provide all the files in the same encoding.
# The following files seem to be _not_ in UTF-8 but in ISO-8859-1, which is why you may want to
# use a text editor first to save them with UTF-8 encoding so that the loop below works perfectly everywhere:
# s1a-061.cor, s1b-060.cor, w2a-002.cor, w2a-004.cor, w2a-007.cor, w2a-036.cor, w2b-002.cor, w2b-037.cor, w2d-016.cor, w2f-003.cor

# define the directory where the ICE-GB files are located and get the file names
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# create an output directory for the cleaned files
dir.create("../_outputfiles/05_18_icegb-output")

for(i in corpus.files) {
   cat(basename(i), "\n") # output a 'progress report'

   current.corpus.file <- scan(i, what=character(), sep="\n", quiet=TRUE) # load the i-th/current corpus file
   current.corpus.file <- paste(current.corpus.file, # paste together the whole file into one vector element
                                collapse="\n")       # with linebreaks between the original elements

   current.sentences <- unlist(     # create current.sentences by changing into a vector the list resulting from
      strsplit(current.corpus.file, # splitting up the one long vector element
               "<sent>")            # at every occurrence of <sent>
   )[-1]                            # and drop the part before the first occurrence

   current.words <- exact.matches.2( # make current.words what you get when looking for
      "(?<={)[^\n\\{\\}]+(?=\\}\n)", # anything with a { to its left, not curly brackets and linebreaks in it, and a } before \n to its right
      current.sentences,             # in current.sentences
      gen.conc.output=FALSE)[1:2]    # don't generate concordances and retain both the matches and their (sentence) locations

   current.sentences <- tapply( # make current sentences the result of applying to
      current.words[[1]],       # the matches (i.e., the words)
      current.words[[2]],       # a grouping by their locations (in the sentences)
      paste, collapse=" ")      # and then paste the grouped words together with spaces

   cat(current.sentences, # print the current sentences
       file=paste0("../_outputfiles/05_18_icegb-output/", basename(i)), # into this file in the output directory and file
       sep="\n")          # with line breaks between sentences
} # end of loop



# excursus 1: this is another way you could use to do what is done in lines 29-37 above:
current.words <- exact.matches.2( # make current.words what you get when looking for
   "(?<={)[^\n\\{\\}]+(?=\\}\n)", # anything with a { to its left, not curly brackets and linebreaks in it, and a } before \n to its right
   current.sentences,             # in current.sentences
   gen.conc.output=FALSE, vectorize=FALSE)[[1]] # suppress concordance output but don't vectorize the matches
current.sentences <- sapply( # make current sentences the result of applying to
   current.words,            # each element of current.words
   paste,                    # a pasting operation
   collapse=" "              # with spaces between the emrged elements
)                            # end of sapply(...)



# excursus 2: with %>%, this is how you could do what is done in lines 20-32 above:
current.words <-
   i                                                 %>%
   scan(what=character(), sep="\n", quiet=TRUE)      %>%
   paste(collapse="\n")                              %>%
   strsplit("<sent>")                                %>%
   unlist                                            %>%
   "["(-1)                                           %>%
   exact.matches.2("(?<={)[^\n\\{\\}]+(?=\\}\n)", .,
                   gen.conc.output=FALSE)            %>%
   "["(1:2)



# excursus 3: add the logic of excursus 1 to that of excursus 2 and this is what could be all that's done in the loop,
# i.e., this could replace all of lines 20-41 (leaves empty character strings in, but otherwise identical)
i                                                       %>%
scan(what=character(), sep="\n", quiet=TRUE)            %>%
paste(collapse="\n")                                    %>%
strsplit("<sent>")                                      %>%
unlist                                                  %>%
"["(-1)                                                 %>%
exact.matches.2("(?<={)[^}]+", .,
                gen.conc.output=FALSE, vectorize=FALSE) %>%
"[["(1)                                                 %>%
sapply(paste, collapse=" ")                             %>%
cat(file=paste0(
   "../_outputfiles/05_18_icegb-output/",
   basename(i)), sep="\n")
