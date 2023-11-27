rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.dir
library(dplyr) # load the library for %>%

# load the CELEX file with English lemma information <.../CELEX2/ENGLISH/EPL/EPL.CD> or its excerpt provided with the book
epl <- scan(file.choose(), what=character(), sep="\n", quote="", comment.char="") # <_qclwr2/_inputfiles/dat_EPL_qclwr2.CD>

# change those epl into a list and extract words, their segmental make-ups, and their pronunciations
epl.list <- strsplit(epl,    # generate epl.list by splitting up epl
                     "\\\\") # at every occurrence of 1 (!) backslash

words <- sapply(epl.list, # generate a vector called words by applying to each element of the list epl.list
                "[",      # the extracton function [
                2)        # to extract each 2nd element
pronuns <- sapply(epl.list, "[", 6)  # for pronunications, do the same and extract the 6th elements

# load the CELEX file with English syntactic information <.../CELEX2/ENGLISH/ESL/ESL.CD> or its excerpt provided with the book
esl <- scan(file.choose(), what=character(), sep="\n", quote="", comment.char="")  # <_qclwr2/_inputfiles/dat_ESL_qclwr2.CD>

# find all adjectives
esl.list <- strsplit(esl,    # generate esl.list by splitting up esl
                     "\\\\") # at every occurrence of 1 (!) backslash

# generate a logical vector that contains TRUEs for all adjectives and FALSEs for all other parts of speech
adjectives <- sapply(esl.list, # generate a vector called adjectives by applying to each element of the list esl.list
                     "[",      # the extracton function [
                     4)=="2"   # to extract each 4th element and check whether it is "2"



# some of the questions you can now answer
# how many English adjectives are there in the database?
sum(adjectives)
# 17 (in the real CELEX database you'd get 9214)



# how many adjectives have 4 syllables?
# this is the traditional nested way
table(sapply(strsplit(pronuns[adjectives], "-"), length))["4"]
# but we now use %>% ...
pronuns[adjectives] %>% # retrieve only those pronunciations that are for adjectives
   strsplit("-")    %>% # split them up at hyphens (which will return a list)
   sapply(length)   %>% # count the number of elements of each element of that list
   table            %>% # tabulate the syllabic lengths
   "["(., "4")          # extract the frequency for syllabic length = 4
# in these files, 2; in the real CELEX database you'd get 1686



# how many adjectives begin with schwa?
pronuns[adjectives]                     %>% # retrieve only those pronunciations that are for adjectives
   grep("^@", ., value=TRUE, perl=TRUE) %>% # find the pronunciations that begin with schwa
   length                                   # count how many such elements there are
# in these files, 1; in the real CELEX database you'd get 227



# which adjectives have the sound sequence corresponding to the word "plea" in them?
pronuns[adjectives]                 %>% # retrieve only those pronunciations that are for adjectives
   gsub("[-'\"]", "", ., perl=TRUE) %>% # delete syllabification info etc.
   grep("pli", ., value=TRUE)           # look for the transcription of "plea"
# in these files, "rIplit"; in the real CELEX database you'd get
# "k@mplit" "dIsplizIN" "g5{zjupliz" "Ink@mplit" "p{r@pli_Ik" "plizd" "plizIN" "plin@rI" "rIplit"



# how frequent are all consonant cluster lengths in adjectives? (ignoring syllabicification for now and ignoring the segment colum)
# generate character vectors that contain all consonant and vowel codes in the DISC transcription
C.codes <- "pbtdkgNmnlrfvTDszSZjxhwJ_CFHPR" # p. 4-25 of <EUG_LET.PS>
V.codes <- "IE{VQU@i#$u312456789cq0~"       # p. 4-26 of <EUG_LET.PS>

# to be able to use chartr, we need character vectors with equally many Cs and Vs
Cs <-                      # create Cs from
   "C"                 %>% # taking the character "C"
   rep(nchar(C.codes)) %>% # repeating it as many times as there a distinct consonsant codes in DISC transcription set
   paste(collapse="")      # and paste those together into one character string
Vs <- "V" %>% rep(nchar(V.codes)) %>% paste(collapse="") # same for V.codes

chartr(V.codes, Vs, chartr(C.codes, Cs, "dIsplizIN"))

# retrieve all adjective consonant cluster lengths without syllabification info
pronuns[adjectives]                 %>% # take the pronunciations of adjectives
   gsub("[-'\"]", "", ., perl=TRUE) %>% # delete syllabification info etc.
   chartr(C.codes, Cs, .)           %>% # replace all consonant phonemes with Cs
   chartr(V.codes, Vs, .)           %>% # replace all vowel phonemes with Vs
   strsplit("V+", perl=TRUE)        %>% # split up the result at every sequence of Vs (creating a list with C sequences)
   sapply(nchar)                    %>% # count the number of Cs in all elements of vectors of the list
   unlist                           %>% # unlist that list
   table                                # and tabulate the resulting vector
# in these files, you get
# 0  1  2  3
# 1 41 10  2
# in the real CELEX database you'd get
#    0     1     2     3     4     5
# 2147 20129  8555  1406   150     3

maximal.C.clusts.per.adj <-pronuns[adjectives] %>% # take the pronunciations of adjectives
   gsub("[-'\"]", "", ., perl=TRUE)            %>% # delete syllabification info etc.
   chartr(C.codes, Cs, .)                      %>% # replace all consonant phonemes with Cs
   chartr(V.codes, Vs, .)                      %>% # replace all vowel phonemes with Vs
   strsplit("V+", perl=TRUE)                   %>% # split up the result at every sequence of Vs (creating a list with C sequences)
   sapply(nchar)                               %>% # count the number of Cs in all elements of vectors of the list
   sapply(max)

words[adjectives][                  # show words that are adjectives, but only those
   sapply(maximal.C.clusts.per.adj, # whose longest sequence of contiguous clusters
          "==",                     # is
          3)                        # 3 (in the real CELEX database you'd use 5)
] # end of second [
# in these files, you get
# "chilblained"  "childbearing"
# in the real CELEX database (with 5 consonants), you'd get
# "second-string" "single-breasted" "smash-and-grab"



# the easier (because we use existing segment information) and
# more realistic way (because we respect syllable boundaries
seggies <- sapply(epl.list, # generate a vector called seggies by applying to each element of the list epl.list
                  "[",      # the extracton function [
                  7)        # # to extract each 7th element

maximal.C.clusts.per.adj <-                # find the maximal number of consonsants per adjectives by
   gregexpr("C+", seggies[adjectives]) %>% # finding 1+ Cs in the segments of the adjectives (returns a list)
   sapply(attr, "match.length")        %>% # extract the lengths of these matches per adjective
   sapply(max)                             # find the maximum numbers of contiguous Cs per adjective

words[adjectives][                  # show words that are adjectives, but only those that
   maximal.C.clusts.per.adj==       # have a maximum numbers of contiguous Cs that corresponds to
      max(maximal.C.clusts.per.adj) # the overall maximum number of contiguous Cs
] # end of second [
# in these files, you get
# "abandoned" "chesty" "chilblained" "childbearing" "replete"
# in the real CELEX database you'd get 304 words from "abstract" to "zonked"
