#### Chapter 5: Using R in corpus linguistics



# (1)   Create a case-insensitive reverse frequency list (of )the kind exemplified in Table 1 in Section 2.2 in the book) of the file <_qclwr2/_inputfiles/corp_gpl_long.txt>. Use dplyr's %>% as much as you can.



# (2)   Some not-so-serious practice with frequency lists ...
#       Download my research website (<http://www.linguistics.ucsb.edu/faculty/stgries/research/overview-research.html>) into a character vector with scan, discard everything before the body of the website starts (i.e., everything before "<body lang="en-US" dir="LTR">"), delete all tags (simplistically just everything between angular brackets, split the whole thing up into words at every occurrence of one or more non-letters (recall Unicode categories?), and then generate a frequency table called words.table. If you'd like, try to do this using dplyr's %>% operator.

library(dplyr)



#       (b) How do you restrict this frequency list to words that begin with an r?



#       (c) How do you restrict this frequency list to words that contain 2+ e's and occur 5+ times?



#       (d) This is very simplistic approach, which has problems with some aspects of the text on my website. For example, why does this happen although I co-authored two papers with Doris?
"Schönefeld" %in% names(words.table)
# [1] FALSE



#       (e) How do you change words.table into a case-insensitive version of it, BUT without changing the above code?



# (3)   Some more nonsense with frequency lists based on a part of my website ...
#       Download my research website (<http://www.linguistics.ucsb.edu/faculty/stgries/research/overview-research.html>) into a character vector with scan, discard everything before the body of the website starts (i.e., everything before "<body lang="en-US" dir="LTR">"), delete all tags (simplistically just everything between angular brackets, split the whole thing up into words at every occurrence of one or more non-letters (recall Unicode categories?), and then generate a frequency table called words.table. If you'd like, try to do this using dplyr's %>% operator. This is the same as before, only change, we now immediately make it case-insensitive.

library(dplyr)



#       (a) Now we are going to compute a linguistically very relevant characteristic of words, namely their e-ness. This important feature measures the proportion of e's out of all characters in the word. So, compute the e-ness of all words that have at least two e's in them and are at least 5 characters long and represent the e-ness values as a function of the word length in a plot.



#       (b) We are now going to compute a linguistically even more important characteristic of words, namely their v-ness. This even more important feature measures the proportion of 'vowel letters' - i.e., a, e, i, o, u - out of all characters in the word. And if you don't think this feature is important, be advised that I was told that a low v-ness value in a name has been used as a heuristic to flag people that try to enter a certain country as potentially suspicious, and I was told that by a colleague who was put on some must-interrogate-for-4-hours list simply because of her last name's low v-ness. So, compute the v-ness of all words that have at least 2 'vowel letters' in them and are at least 5 characters long and represent the v-ness values as a function of the word length in a plot.



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



# (5)   A follow-up on indexing exercise 3 (<05_19c_indexing.r>): If you run that code, you will see that it works without problems in the example discussed in Section . However, there is a catch there: Lines 58-70 work perfectly in that example, but there are applications where they will not work well. Therefore, try to look at the code and answer the following questions:
#       (a) Can you see from lines 58-70 in the code in what situation the script will NOT produce the desired results?
#       (b) Obviously, can you imagine a way to fix this problem? (It can be done with a single line of code with <40 characters)



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



# (7)   Another indexing exercise, but this time one where the characters occurring in 'the book' make it harder or even impossible to define the words to index by putting "\\b" around them. We are going to work with three pages from a recent book by Marlies Jansegers, to whom I am grateful for allowing me to use her work as an example. The task is very similar to the ones in <05_19a_indexing.r>, <05_19b_indexing.r>, and <05_19c_indexing.r> so you will be able to recycle _a lot_ of the code there. Write a script that does two things:
#       (a) It creates a case-insensitive index for all 'words' occurring in the file <_qclwr2/_inputfiles/dat_MJ-book.txt>; make sure ranges of neighboring pages are collapsed (recall exercise #5 above for that), and make sure you get the definition of words right because you cannot rely on "\\b".
#       (b) It creates an index for a few author names (names2index <- c("Geeraerts", "Hilferty", "Ibarretxe-Antuñano", "Valenzuela", "Weisgerber")).



# (8)   A follow-up on the verb-collexemes-after-must exercise in <05_14_must-V.r>: If you run that code, you will see that it runs without errors but, as Alvin Chen and a student of his observed (I thank them for letting me know), it can return frequencies that are slightly off because of how the script tries to determine the frequencies of the infinitives after the modal verbs. Specifically, the script
#       - loads a corpus file and retrieves its sentences, which are stored in current.sentences;
#       - finds all sequences of a modal verb and an infinitive, which are stored in current.mpis'
#       - pastes together a search expression using the lemma attribute-value pair, which is stored in current.search.expression;
#       - looks for that search expression in current.mpis.
#       This means, when j becomes freqs.overall[349], it becomes "need" (as an infinitive, not as a modal!), and then it of course does not just find "need" when it's a vvi after a modal, but also when it's the modal itself. Your task is to correct that mistake so that the input to the collexeme analysis table, the data frame result, lists not 540 instances of "need" in the corpus but just 516. If you do it cleverly, you only need to insert one line of code into the script and can even keep the line numbering in sync with the current one.
