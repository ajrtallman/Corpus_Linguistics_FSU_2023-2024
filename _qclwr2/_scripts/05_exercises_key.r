#### Chapter 5: Using R in corpus linguistics



# (1)   Create a case-insensitive reverse frequency list (of )the kind exemplified in Table 1 in Section 2.2 in the book) of the file <_qclwr2/_inputfiles/corp_gpl_long.txt>. Use dplyr's %>% as much as you can.

tab <- scan(file.choose(), what=character(), sep="\n") %>% # choose and load <qclwr2/_inputfiles/corp_gpl_long.txt>
   tolower                                 %>% # change it to lower case
   strsplit("[^a-z]+", perl=TRUE)          %>% # split it up at anything not a letter from a-z
   unlist                                  %>% # unlist that list into a vector
   "["(., nzchar(.))                       %>% # retain only elements with at least one character
   table                                       # and tabulate

# without dplyr, we would have written the above like this:
# txt <- tolower(scan(file.choose(), what=character(), sep="\n"))
# wrds <- unlist(strsplit(txt, "[^a-z]+", perl=TRUE))
# tab <- table(words[nzchar(wrds)])
# works just as well but I know what I prefer to look at ...

tab.rev <- tab                # create a copy of tab
names(tab.rev) <-             # make the new names of tab.rev what you get when you take
   names(tab)             %>% # the names of tab
   strsplit("")           %>% # split them up character-by-character (yielding a list)
   lapply(rev)            %>% # apply rev to each element of that list
   sapply(paste, collapse="") # apply paste with collapse="" to each element of that list

# without dplyr, we would have written the above like this:
# names(tab.rev) <- sapply(lapply(strsplit(names(tab.rev), ""), rev), paste, collapse="")
# works just as well but I know what I prefer to look at ...

order.index <- order(names(tab.rev))
(tab.rev <- tab.rev[order.index])



# (2)   Some not-so-serious practice with frequency lists ...
#       Download my research website (<http://www.linguistics.ucsb.edu/faculty/stgries/research/overview-research.html>) into a character vector with scan, discard everything before the body of the website starts (i.e., everything before "<body lang="en-US" dir="LTR">"), delete all tags (simplistically just everything between angular brackets, split the whole thing up into words at every occurrence of one or more non-letters (recall Unicode categories?), and then generate a frequency table called words.table. If you'd like, try to do this using dplyr's %>% operator.

library(dplyr)
website <- scan(
   "http://www.linguistics.ucsb.edu/faculty/stgries/research/overview-research.html",
   what=character(), sep="\n", quiet=TRUE) %>%
   "["(., min(grep("<body", .)):length(.)) %>%
   gsub("<.*?>", "", ., perl=TRUE)

words.table <-
   website                         %>%
   strsplit("")                    %>%
   unlist                          %>%
   table                           %>%
   names                           %>%
   exact.matches.2("\\p{L}", .)    %>%
   "[["(1)                         %>%
   paste(collapse="")              %>%
   paste("[^", ., "]+", sep="")    %>%
   strsplit(website, ., perl=TRUE) %>%
   unlist                          %>%
   table                           %>%
   sort
tail(words.table, 30)



#       (b) How do you restrict this frequency list to words that begin with an r?
words.table[grep("^r", names(words.table), ignore.case=TRUE, perl=TRUE)] # flexible approach



#       (c) How do you restrict this frequency list to words that contain 2+ e's and occur 5+ times?
words.table[
   grepl("e.*e", names(words.table), ignore.case=TRUE, perl=TRUE)
   &
   words.table>=5
]



#       (d) This is very simplistic approach, which has problems with some aspects of the text on my website. For example, why does this happen although I co-authored two papers with Doris?
"Schönefeld" %in% names(words.table)
# [1] FALSE



#       (e) How do you change words.table into a case-insensitive version of it, BUT without changing the above code?
words.table.ci <- words.table # create a copy of word.table (to make case-insensitive)
names(words.table.ci) <- tolower(names(words.table.ci)) # change the names of the frequencies to lower case
words.table.ci <- as.table(      # make a table
   tapply(words.table.ci,        # the result of taking the frequencies
          names(words.table.ci), # grouping them by the words
          sum)                   # and summing them up per word/group
) # end of as.table




# (3)   Some more nonsense with frequency lists based on a part of my website ...
#       Download my research website (<http://www.linguistics.ucsb.edu/faculty/stgries/research/overview-research.html>) into a character vector with scan, discard everything before the body of the website starts (i.e., everything before "<body lang="en-US" dir="LTR">"), delete all tags (simplistically just everything between angular brackets, split the whole thing up into words at every occurrence of one or more non-letters (recall Unicode categories?), and then generate a frequency table called words.table. If you'd like, try to do this using dplyr's %>% operator. This is the same as before, only change, we now immediately make it case-insensitive.

library(dplyr)
website <- scan(
   "http://www.linguistics.ucsb.edu/faculty/stgries/research/overview-research.html",
   what=character(), sep="\n", quiet=TRUE) %>%
   "["(., min(grep("<body", .)):length(.)) %>%
   gsub("<.*?>", "", ., perl=TRUE)

words.table <-
   website                         %>%
   strsplit("")                    %>%
   unlist                          %>%
   table                           %>%
   names                           %>%
   exact.matches.2("\\p{L}", .)    %>%
   "[["(1)                         %>%
   paste(collapse="")              %>%
   paste("[^", ., "]+", sep="")    %>%
   strsplit(website, ., perl=TRUE) %>%
   unlist                          %>%
   tolower                         %>%
   table                           %>%
   sort
tail(words.table, 30)

#       (a) Now we are going to compute a linguistically very relevant characteristic of words, namely their e-ness. This important feature measures the proportion of e's out of all characters in the word. So, compute the e-ness of all words that have at least two e's in them and are at least 5 characters long and represent the e-ness values as a function of the word length in a plot.

e.words <- names(words.table[
   grepl("e.*e", names(words.table), perl=TRUE)
   &
   words.table>=5
])

e.ness <- sapply(
   gregexpr("e",
            e.words),
   length) /
   nchar(e.words)
names(e.ness) <- e.words; sort(e.ness)

plot(nchar(e.words), e.ness, type="n",
     xlim=c(2, 16), ylim=c(0, 2/3),
     xlab="Length of words with 2 e's", ylab="E-ness"); grid()
   text(nchar(e.words),
        runif(length(e.ness), 0, 0.07)+e.ness, # more than the normal jitter is needed here
        e.words,
        cex=0.8)
   lines(lowess(e.ness ~ nchar(e.words)), col="pink", lwd=4)



#       (b) We are now going to compute a linguistically even more important characteristic of words, namely their v-ness. This even more important feature measures the proportion of 'vowel letters' - i.e., a, e, i, o, u - out of all characters in the word. And if you don't think this feature is important, be advised that I was told that a low v-ness value in a name has been used as a heuristic to flag people that try to enter a certain country as potentially suspicious, and I was told that by a colleague who was put on some must-interrogate-for-4-hours list simply because of her last name's low v-ness. So, compute the v-ness of all words that have at least 2 'vowel letters' in them and are at least 5 characters long and represent the v-ness values as a function of the word length in a plot.

v.words <- names(words.table[
   grepl("[aeiou].*[aeiou]", names(words.table), perl=TRUE)
   &
   words.table>=5
])

v.ness <- sapply(
   gregexpr("[aeiou]", v.words, perl=TRUE),
   length) /
   nchar(v.words)
names(v.ness) <- v.words; sort(v.ness)

plot(nchar(v.words), v.ness, type="n",
     xlim=c(1, 20), ylim=c(0, 1),
     xlab="Length of words with 'vowels'", ylab="Vowel-ness"); grid()
   text(jitter(nchar(v.words)),
        runif(length(v.ness), 0, 0.08)+v.ness, # more than the normal jitter is needed here
        v.words,
        cex=0.75, srt=45)
   lines(lowess(v.ness ~ nchar(v.words)), col="pink", lwd=4)
   qwe <- tapply(v.ness, nchar(v.words), mean)
   lines(as.numeric(names(qwe)), qwe, col="blue", lwd=4)



# (4)   A follow-up on playing with CELEX
#       In Section 5.3.4, we found the number of adjectives with 4 syllables (2). I now want you do find out which adjectives those are. Here is the code with which we loaded and prepared everything ...
   rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs); library(dplyr)

# load the CELEX file with English lemma information <.../CELEX2/ENGLISH/EPL/EPL.CD> or its excerpt provided with the book
epl <- scan(file.choose(), what=character(), sep="\n", quote="", comment.char="") # <_qclwr2/_inputfiles/dat_EPL_qclwr2.CD>
# load the CELEX file with English syntactic information <.../CELEX2/ENGLISH/ESL/ESL.CD> or its excerpt provided with the book
esl <- scan(file.choose(), what=character(), sep="\n", quote="", comment.char="")  # <_qclwr2/_inputfiles/dat_ESL_qclwr2.CD>

# change those epl into a list and extract words, and their pronunciations
epl.list <- strsplit(epl, "\\\\")
words <- sapply(epl.list, "[", 2)
pronuns <- sapply(epl.list, "[", 6)

# generate a logical vector that contains TRUEs for all adjectives and FALSEs for all other parts of speech
esl.list <- strsplit(esl, "\\\\")
adjectives <- sapply(esl.list, "[", 4)=="2"

# ... now, what are these two adjective types?
pronuns[adjectives] %>%
   gregexpr("-", .) %>%
   sapply(length)   %>%
   "=="(3)          %>%
   "["(words[adjectives], .)
# [1] "chickenhearted"  "chicken-livered"



# (5)   A follow-up on indexing exercise 3 (<05_19c_indexing.r>): If you run that code, you will see that it works without problems in the example discussed in Section . However, there is a catch there: Lines 58-70 work perfectly in that example, but there are applications where they will not work well. Therefore, try to look at the code and answer the following questions:

#      (a) Can you see from lines 58-70 in the code in what situation the script will NOT produce the desired results?
# Lines 58-70 will produce problematic results when a certain index term has only a single page number, in which case its last character will be deleted. You can check this by putting this code in line 56 of <05_19c_indexing.r>:
index[["z"]] <- 1; index[["zz"]] <- 12; index[["zzz"]] <- 123; index
# and then running the rest of the script.

#      (b) Obviously, can you imagine a way to fix this problem? (It can be done with <40 characters)
# Make this line 58 of <05_19c_indexing.r>:
for(i in seq(index)) { if (length(index[[i]])==1) { next } # for the i-th element of the list called index, *IF* there is more than one page number



# (6)   This is a data-munging exercise in which a certain kind of irregular input format is supposed to be converted into our nice usual case-by-variable format. (I am grateful to Benedikt Heller for this exercise idea.) The data describe 5 cars (in the rows) according to 4 features they possess; the format of the data looks like this and the input file is <_qclwr2/_inputfiles/dat_cardata_input.csv>.

#   ID feature.1 feature.2 feature.3 feature.4
# 1  1       ABS Bluetooth   Keyless        AC
# 2  2       ABS     Radio        AC   Keyless
# 3  3        AC   Leather       ABS        TV
# 4  4     Radio   Leather Bluetooth       ABS
# 5  5     Alarm       ABS     Radio   Leather

# The desired output (to be saved into <_qclwr2/_inputfiles/dat_cardata_output.csv>) is this: The rows are still the 5 cars, but now there is a column for every car feature attested at least once in the data and the cells of the data frame state whether a certain car has a feature or not:

#    ABS    AC Alarm Bluetooth Keyless Leather Radio    TV
# 1 TRUE  TRUE FALSE      TRUE    TRUE   FALSE FALSE FALSE
# 2 TRUE  TRUE FALSE     FALSE    TRUE   FALSE  TRUE FALSE
# 3 TRUE  TRUE FALSE     FALSE   FALSE    TRUE FALSE  TRUE
# 4 TRUE FALSE FALSE      TRUE   FALSE    TRUE  TRUE FALSE
# 5 TRUE FALSE  TRUE     FALSE   FALSE    TRUE  TRUE FALSE

# Write a script that can make this conversion.

rm(list=ls(all=TRUE))
x <- read.delim(file.choose())       # read input from <_qclwr2/_inputfiles/dat_cardata_input.csv>
features <- levels(unlist(x[,-1])) # ... extract features

# generate a matrix xx to collect the results
xx <- matrix(                  # make xx a matrix that
   rep(FALSE,            # contains as many instances of FALSE as there are
       length(features)* # combinations of (here 8) features and
          nrow(x)        # (here 5) items that may have those features
       ),                # end of rep(...)
   ncol=length(features),      # and that has as many columns as there are features
   dimnames=list(seq(nrow(x)), # and sequential ID numbers for items as row names
                 features))    # and features as column names

# fill collector structure row by row
for (i in seq(nrow(xx))) { # access each row of the result matrix (i.e., each item of the input matrix)
   xx[i,] <- features %in% # make that row the TRUEs/FALSEs stating whether the features are
      unlist(x[i,-1])      # in the (vectorized) elements of that row
}                          # end of for (...)

# check and save output (ultimately into <_qclwr2/_outputfiles/dat_cardata_output.csv>)
xx
write.table(xx, file="dat_cardata_output.csv", sep="\t",
            row.names=TRUE, col.names=NA, quote=FALSE)



# (7)   Another indexing exercise, but this time one where the characters occurring in 'the book' make it harder or even impossible to define the words to index by putting "\\b" around them. We are going to work with three pages from a recent book by Marlies Jansegers, to whom I am grateful for allowing me to use her work as an example. The task is very similar to the ones in <05_19a_indexing.r>, <05_19b_indexing.r>, and <05_19c_indexing.r> so you will be able to recycle _a lot_ of the code there. Write a script that does two things:
#       (a) It creates a case-insensitive index for all 'words' occurring in the file <_qclwr2/_inputfiles/dat_MJ-book.txt>; make sure ranges of neighboring pages are collapsed (recall exercise #5 above for that), and make sure you get the definition of words right because you cannot rely on "\\b".

rm(list=ls(all=TRUE)) # clear memory

# load the data
book.file <- readLines(con <- file( # read in lines from a file connection to
   file.choose(),                   # <_qclwr2/_inputfiles/dat_MJ-book.txt>
   encoding="UTF-8"),               # which is in UTF-8 encoding
   warn=FALSE); close(con)          # suppress warnings and close the connection

book.file.one.string <- paste(book.file,    # paste the vector book.file into one character string
                              collapse=" ") # with spaces between the original vector elements

book.file.pages <- unlist(strsplit( # unlist the list you get from splitting
   book.file.one.string,            # the 1-element character vector book.file.one.string
   " *24\\.10\\.16 08:4[12] *",     # at a code that is occurs once at the bottom of each page
   perl=TRUE                        # using Perl-compatible regular expressions
   ))[-1]                           # and omit first empty string


# find all characters to define what characters to split on
(characters <- names(                  # output the result of making characters the names of
   table(                              # the table of
      unlist(                          # the unlisted/vectorized output of
         strsplit(book.file.pages, "") # strplitting the book character by character
      ) # end of unlist(...)
   )    # end of table(...)
))      # end of names(...) and output
characters.2.split <- paste0( # define a regex characters.2.split by pasting without separators
   "[^-",                     # a character class of not-the-following-things incl. the hyphen
   paste0(                    # and the pasting-together of
      characters[30:79],      # characters 30-79
      collapse=""),           # without any intervecning characters
   "]+", collapse="")         # then end the character class and add "one or more"

# identify word tokens and types
words.tokens <- unlist(   # make words.tokens the result of unlisting/vectorizing
   strsplit(              # the result of strplitting
      book.file.pages,    # the book
      characters.2.split, # by the characters to use for splitting
      perl=TRUE)          # using Perl-compatible regular expressions
) # end of unlist(...)

word.types.1 <- sort(unique(words.tokens)) # sort all word types

# remove numbers
word.types.2 <- gsub("^\\d+(\\.\\d+)?$", # replace numbers
                     "",                 # by nothing
                     word.types.1,       # in the word types
                     perl=TRUE)          # using Perl-compatible regular expressions

# remove empty character strings
word.types.3 <- sort(                       # sort
   unique(                                  # the unique types
      tolower(                              # when you changed to lower case
         word.types.2[nzchar(word.types.2)] # all character strings that are not empty
      ) # end of tolower(...)
   ) # end of unique(...)
) # end of sort(...)

# create overall index
index <- list()
index[word.types.3] <- lapply( # apply to
   paste(             # the pasting together of
      "[^\\p{L}]",          # anything that's not a letter from any language
      word.types.3,         # the word types just created
      "[^\\p{L}]", sep=""), # anything that's not a letter from any language, without a separator
   grep,              # the function grep, i.e. searching (for position indices)
   x=book.file.pages, # searching in the character vector book.file.pages
   perl=TRUE,         # using Perl-compatible regular expressions
   ignore.case=TRUE)  # ignore case

# merging ranges
for(i in seq(index)) { # for the i-th element of the list called index
   if (length(index[[i]])==1) { next }   # if there's only one page number, don't do anything
   ranges <- ifelse(diff(index[[i]])==1, # if the difference between neighboring numbers is 1
                    "-",                 # store a "-",
                    ",")                 # otherwise a ","
   all.in.one <- paste(index[[i]], # paste the index numbers together with
                       ranges,     # the above elements from ranges
                       sep="", collapse=" ") # no intervening materials, all into one string
   all.in.one <- substr(all.in.one, 1, nchar(all.in.one)-1) # omit the last character of all.in.one
   index[[i]] <- gsub("(\\d+)- (\\d+- )*(\\d+)", # replace (sequences of numbers) followed be "-" and some last number
                      "\\1-\\3",                 # by the first and the last number, with a "-" inbetween
                      all.in.one,                # in the character vector all.in.one
                      perl=TRUE)                 # using Perl-compatible regular expressions
}

# output the index
cat(paste(names(index), index, sep="\t"), sep="\n", file="dat_MJ-index.txt")

#       (b) It creates an index for a few author names (names2index <- c("Geeraerts", "Hilferty", "Ibarretxe-Antuñano", "Valenzuela", "Weisgerber")).

# create a vector with names for which we want index pages
names2index <- c("Geeraerts", "Hilferty", "Ibarretxe-Antuñano", "Valenzuela", "Weisgerber")

# create overall index
index <- list()
index[names2index] <- lapply( # apply to
   paste(             # the pasting together of
      "[^\\p{L}]",          # anything that's not a letter from any language
      names2index,          # the author names to be indexed
      "[^\\p{L}]", sep=""), # anything that's not a letter from any language, without a separator
   grep,              # the function grep, i.e. searching (for position indices)
   x=book.file.pages, # searching in the character vector book.file.pages
   perl=TRUE,         # using Perl-compatible regular expressions
   ignore.case=TRUE)  # ignore case

# merging ranges
for(i in seq(index)) { # for the i-th element of the list called index
   if (length(index[[i]])==1) { next }   # if there's only one page number, don't do anything
   ranges <- ifelse(diff(index[[i]])==1, # if the difference between neighboring numbers is 1
                    "-",                 # store a "-",
                    ",")                 # otherwise a ","
   all.in.one <- paste(index[[i]], # paste the index numbers together with
                       ranges,     # the above elements from ranges
                       sep="", collapse=" ") # no intervening materials, all into one string
   all.in.one <- substr(all.in.one, 1, nchar(all.in.one)-1) # omit the last character of all.in.one
   index[[i]] <- gsub("(\\d+)- (\\d+- )*(\\d+)", # replace (sequences of numbers) followed be "-" and some last number
                      "\\1-\\3",                 # by the first and the last number, with a "-" inbetween
                      all.in.one,                # in the character vector all.in.one
                      perl=TRUE)                 # using Perl-compatible regular expressions
}

# output the index
cat(paste(names(index), index, sep="\t"), sep="\n")



# (8)   A follow-up on the verb-collexemes-after-must exercise in <05_14_must-V.r>: If you run that code, you will see that it runs without errors but, as Alvin Chen and a student of his observed (I thank them for letting me know), it can return frequencies that are slightly off because of how the script tries to determine the frequencies of the infinitives after the modal verbs. Specifically, the script
#       - loads a corpus file and retrieves its sentences, which are stored in current.sentences;
#       - finds all sequences of a modal verb and an infinitive, which are stored in current.mpis'
#       - pastes together a search expression using the lemma attribute-value pair, which is stored in current.search.expression;
#       - looks for that search expression in current.mpis.
#       This means, when j becomes freqs.overall[349], it becomes "need" (as an infinitive, not as a modal!), and then it of course does not just find "need" when it's a vvi after a modal, but also when it's the modal itself. Your task is to correct that mistake so that the input to the collexeme analysis table, the data frame result, lists not 540 instances of "need" in the corpus but just 516. If you do it cleverly, you only need to insert one line of code into the script and can even keep the line numbering in sync with the current one.

# Make this line 80 of <05_14_must-V.r>
   current.mpis <- gsub("^.*?</w>", "", current.mpis, perl=TRUE) # delete the modal verb parts from current.mpis to retain only the infinitives
