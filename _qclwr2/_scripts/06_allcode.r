#### Chapter 6: Next steps ...

rm(list=ls(all=TRUE))
library(stringi)

example.vector <- c("This is a first example sentence", "And this is a second example sentence")

# overview statistics of a character vector
stri_stats_general(example.vector)

# grepl
# grepl("is", example.vector)
stri_detect(example.vector, regex="is")

# how often does the regex occur in each element of vector.to.be.searched?
# sapply(gregexpr("is", example.vector), length)
stri_count(example.vector, regex="is")

# substr
# substr(example.vector, 1, 3)
stri_sub(example.vector, 1, 3)

# nchar
# nchar(example.vector)
stri_length(example.vector)

# trim leading and trailing whitespace
# gsub("(^ +| +$)", "", "  qwerty ", perl=TRUE)
stri_trim(" qwerty  ")

# just.matches
# qwe <- exact.matches.2("is", example.vector); tapply(qwe[[1]], qwe[[2]], c)
stri_extract_all(example.vector, regex="is")

stri_extract_first(example.vector, regex="..")
stri_extract_last(example.vector, regex="..")

# paste
stri_join(1:3, 4:6, sep="_")    # same as paste
stri_flatten(1:3, collapse="_") # same as paste
# check also ?stri_dup

# an example of encoding detection
# load <_qclwr2/inputfiles/corp_coralrom/ifamdl02.txt>
coralrom.1 <- readLines(con <- file(file.choose(), encoding="ISO-8859-15"),
                                     warn=FALSE); close(con)

encoding.voter <- function(input) {
   require(stringi); require(dplyr)
   detector <- stri_enc_detect(input)
   sapply(sapply(detector, "[", 3), function(x) x==max(x)) %>%
      mapply("[", sapply(detector, "[", 1), .)             %>%
      unlist                                               %>%
      table                                                %>%
      sort                                                 %>%
      "["(., which(.==max(.)))
}
encoding.voter(coralrom.1) # does better than Encoding(coralrom.1)

detach(package:stringi)
library(stringr)

# str_c (see paste)
# paste(example.vector, sep="_")
str_c(example.vector, sep="_")
# paste(example.vector, collapse="_")
str_c(example.vector, collapse="_")

# paste(example.vector, example.vector, sep="_")
str_c(example.vector, example.vector, sep="_")
# paste(example.vector, example.vector, sep="", collapse="_")
str_c(example.vector, example.vector, collapse="_")

# nchar
# nchar(example.vector)
str_length(example.vector)

# how often does the regex occur in each element of vector.to.be.searched?
# sapply(gregexpr("is", example.vector), length)
str_count(example.vector, "is")

# str_detect
# grepl("is", example.vector)
str_detect(example.vector, "is")
# grepl("T", example.vector)
str_detect(example.vector, "T")

# str_dup
# paste0(example.vector, example.vector)
str_dup(example.vector, 2)
str_dup(example.vector, 2:3)

# str_extract_all and str_match_all (not discussing str_extract and str_match)
shopping_list <- c("apples x4", "flour", "sugar", "milk x2")
str_extract_all(shopping_list, "p") # similar to exact.matches("p", shopping_list) # with [[1]] and [[2]]
str_match_all(shopping_list, "[a-z]*(l.)[a-z]*")

# regmatches(example.vector, gregexpr("is", example.vector)) # or you can of course use exact.matches.2 again
str_extract_all(example.vector, "is")
# complete match, then each captured/parenthesized group (comparable to XXX in the gsubfn package below)
str_match_all(example.vector, "([a-z]*)([a-z]s)[a-z]*")

# str_locate_all (not discussing str_locate)
# similar to gregexpr("a", fruit) # (returns starts and ends, not starts and lengths)
str_locate_all(example.vector, "is")

# str_pad and str_trim
str_pad("abcdef", 10, "left", " ")
str_pad("abcdef", 10, "both", " ")

# trim leading and trailing whitespace
# gsub("(^ +| +$)", "", " qwerty  ", perl=TRUE)
str_trim(" qwerty  ")

# str_replace_all
# gsub("[a-b]", "_", example.vector)
str_replace_all(example.vector, "[a-b]", "_")

# str_split
# strsplit(example.vector, " ")
str_split(example.vector, " ")

# str_sub
# substr(example.vector, 6, 13)
str_sub(example.vector, 6, 13)

detach(package:stringr)
library(gsubfn)

# find a pattern in a string and apply a function to it
strapply(example.vector,  # find in example.vector
         "[a-zA-Z]*s\\b", # letter sequences ending in an s
         nchar,           # return the lengths of these sequences
         perl=TRUE)       # using Perl-compatible regular expressions

# find a pattern in a string, apply a function to it, and replace the found string with the result of the function
gsubfn("is",           # find the pattern "is"
       toupper,        # and change it to upper case
       example.vector) # in example.vector

gsubfn("([a-zA-Z]*s)\\b", # find the pattern "[a-zA-Z]*s" and memorize the matched string as \\1
       paste0("<\\1>"),   # paste angular brackets around the memorized
       example.vector,    # in example.vector
       perl=TRUE)         # using Perl-compatible regular expressions

detach(package:gsubfn)
library(stringdist)

example.vector <- c("go", "goes", "going", "went", "gone")

# stringdist: how different is the first argument to each element of the second?
dists.1 <- stringdist("boing", example.vector)
   names(dists.1) <- example.vector
   sort(dists.1)

# also nice: the function adist from R's base package, here answering the question
# "how do you get from "gone" to boing?"
adist("gone", "boing", counts=TRUE)
# a substitution: "g" -> "b"
# a match that you maintain: "o"
# an insertion: "" -> "i"
# a match that you maintain: "n"
# a substitution: "e" -> "g"

# determine which of the following spellings of "separate" involve 2+ mistakes:
seps <- c("separate", "seperate", "separete", "seperete", "seprit")
seps[stringdist(seps, "separate")>=2]

# stringsim: how different (normalized against maximal distance) is the first argument to each element of the second?
dists.2 <- stringsim("boing", example.vector)
   names(dists.2) <- example.vector
   sort(dists.2)

# stringdistmatrix: how different are all vector elements to each other (as a symmetric matrix)
as.dist(stringdistmatrix(example.vector, useNames="strings"), diag=TRUE, upper=TRUE)

# ain
stringdist("boing", example.vector)
ain("boing", example.vector, maxDist=2) # TRUE because of "going"

# amatch
amatch("boing", example.vector, maxDist=1) # 3, because that's where "going" is
amatch("goat", example.vector, maxDist=2) # 1 because it takes two additions to change "go" into "goat"
amatch("bone", example.vector, maxDist=2) # 2 because it takes 1(<2) subsititions to change "gone" into "bone"

# qgrams (recall Section 5.2.1?)
qgrams("channel", q=2)
attr(qgrams("channel", q=2), "dimnames")[[2]]

# use the function qgrams to compute the Dice coefficient for the similarity of the strings "tunnel" and "channel". The Dice coefficient is the percentage of character bigrams two strings share out of all their character bigrams.
w1 <- attr(qgrams("tunnel", q=2), "dimnames")[[2]]
w2 <- attr(qgrams("channel", q=2), "dimnames")[[2]]
2*length(intersect(w1, w2)) / length((c(w1, w2)))



rm(list=ls(all=TRUE)); set.seed(1)
paste(
   paste0(sample(letters)[c(17, 24, 19, 19, 13)], collapse=""),
   "R",
   paste0(sample(letters)[c(20, 26, 9, 25, 26, 1, 6, 6, 19, 17, 25)], collapse=""),
   "!",
   collapse="")
