#### Chapter 3: A brief introduction to R



### Exercise box 3.1: Handling vectors ############################################################
# (1)   How would you load the content of <_qclwr2/_inputfiles/dat_vector-b.txt> into a vector x (line by line), add "This is the third line" as a third element to a new vector x.2, and save the new vector x.2 under the name <_qclwr2/_outputfiles/dat_vector-b.txt> such that each sentence gets its own line?
x <- scan(file.choose(), what=character(), sep="\n") # <_qclwr2/_inputfiles/dat_vector-b.txt>
x.2 <- c(x, "This is the third line")
cat(x.2, file="../_outputfiles/dat_vector-b.txt", sep="\n")



# (2)   Generate a vector m with the small letters from a to j (in alphabetical order).
m <- letters[1:10] # or of course the long variant:
m <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")



# (3)   Generate a vector n with the small letters from a to j (in random order).
n <- sample(letters[1:10])



# (4)   Determine whether letters in n are at the same positions as in m. How many such cases do you find. Which letters are these?
table(m==n)
m[m==n]



# (5)   Save the vector n into a file called <_qclwr2/_outputfiles/dat_vector-c.txt> such that each letter is on one line.
cat(n, file="../_outputfiles/dat_vector-c.txt", sep="\n")



# (6)   How can you paraphrase the function union(x, y) in R (with two imaginary vectors x and y)?
unique(c(x, y))



# (7)   Add the numbers from 1 to 50 to the numbers 100 to 51 respectively.
1:50+100:51



# Now for some more practical/useful corpus-linguistic applications ...

# (8)   On how to use R for lemmatization. First, let's assume you have a small list of verb forms and their lemmas. Generate a vector forms with the following content "do", "does", "did", "done", "doing", "make", "makes", "made", "making", "think", "thinks", "thinking", "thought". Generate a vector lemmas with the following content: "DO", "DO", "DO", "DO", "DO", "MAKE", "MAKE", "MAKE", "MAKE", "THINK", "THINK", "THINK", "THINK". Second, let's now assume you have retrieved verbs from a corpus; generate a vector data with the following content: "did", "hung", "thought", "made".
forms <- c("do", "does", "did", "done", "doing", "make", "makes", "made", "making", "think", "thinks", "thinking", "thought")
lemmas <- c("DO", "DO", "DO", "DO", "DO", "MAKE", "MAKE", "MAKE", "MAKE", "THINK", "THINK", "THINK", "THINK")
data <- c("did", "hung", "thought", "made")

#       (a) How do you find for which forms in data you have no lemma information?
data %in% forms              # or
data[data %in% forms==FALSE] # or
data[!(data %in% forms)]     # or
setdiff(data, forms)

#       (b) How do you generate a vector that contains (i) all lemmas for the verb forms in the vector data for which you have lemmas in the vector lemmas and (ii) NA for the verb forms for which you have no lemmas yet?
lemmas[match(data, forms)]



# (9)   On how to use R for spelling homogenization. First, let's assume you have a list of all Old and Middle English verb forms from a diachronic corpus. Generate a vector all.forms with the following small part of that list: "appear", "appere", "forfeit", "forfeite", "speak", "speake", "understand", "vnderstand", "vnderstood". Second, in order to homogenize the spelling of all verb forms in the corpus, you also built a list of what the Modern English forms of the Old and Middle English forms are. Generate a vector all.homogenizer with the following content: "appear", "appear", "forfeit", "forfeit", "speak", "speak", "understand", "understand", "understood".
all.forms <- c("appear", "appere", "forfeit", "forfeite", "speak", "speake", "understand", "vnderstand", "vnderstood")
all.homogenizer <- c("appear", "appear", "forfeit", "forfeit", "speak", "speak", "understand", "understand", "understood")

# Let's now assume you have a vector corpus.data with the elements "vnderstood", "appear", "speake", "appere". Generate a vector verb.forms containing the Modern English spellings of the data in corpus.data.
corpus.data <- c("vnderstood", "appear", "speake", "appere")
all.homogenizer[match(corpus.data, all.forms)]



# (10)   Imagine you have two vectors a and b such that
a <- c("d", "d", "j", "f", "e", "g", "f", "f", "i", "g")
b <- c("a", "g", "d", "f", "g", "a", "f", "a", "b", "g")

# Of these vectors, you can create frequency lists by writing
(freq.list.a <- table(a)); (freq.list.b <- table(b))
rm(a, b)

# how do you generate the following table joint.freqs only from freq.list.a and freq.list.b, i.e., without any reference to a and b themselves?
# joint.freqs
# a b d e f g i j
# 3 1 3 1 5 5 1 1
 # Before you complain about this question as being unrealistic, consider the possibility that someone sent you two frequency list files of corpora you don't have access to but that you want to merge ...

# approach 1: you generate an empty vector joint.freqs
# (i) that is as long as there are different types in both a and b ...
joint.freqs <- numeric(length=length( # generate a vector joint.freqs that is as long
   unique(                            # as there are unique types
      c(names(freq.list.a),           # in the combination of the types of corpus a ...
        names(freq.list.b))           # ... and corpus b
   ) # end of unique
)) # end of length(...) and vector(...)

# ... and (ii) whose elements have these different types as names
names(joint.freqs) <- unique( # give these vector elements names, namely the unique
   c(names(freq.list.a),      # types from the combination of corpus a ...
     names(freq.list.b))      # ... and corpus b
)

# the elements of the new vector joint.freqs that have the same names as the frequencies in the first frequency list are assigned the respective frequencies
joint.freqs[          # take the elements of joint.freqs
   names(freq.list.a) # that have names of words in corpus a
   ] <- freq.list.a   # and insert the frequencies these words have in corpus a

# The elements of the new vector joint.freqs that have the same names as the frequencies in the second frequency list are assigned the sum of the values they already have (either the ones from the first frequency list or just zeroes) and the respective frequencies
joint.freqs[                              # take the elements of joint.freqs
   names(freq.list.b)                     # that have names of words in corpus b
   ] <- joint.freqs[names(freq.list.b)] + # and add to their already stored frequencies (from corpus a) the frequencies these words have in corpus b
        freq.list.b

# look at the result
joint.freqs

# approach 2 (proposed by Claire Crawford during a bootcamp) uses a function which will only be introduced later in the book, but we will use this approach often later. It's not the fastest, but one that generalized best to other applications
# first the two frequency lists are merged into a single vector ...
freq.list.a.b <- c(freq.list.a, freq.list.b)

# ... and then the sums of all numbers that share the same names are computed
joint.freqs <- as.table(    # make joint freqs a table version of
   tapply(                  # what you get when you group
      freq.list.a.b,        # the frequencies in freq.list.a.b
      names(freq.list.a.b), # by the names of these frequencies (i.e., the words)
      sum))                 # and compute a sum for each group
joint.freqs # look at the result

# approach 3 is similar to approach 2 but more specialized and a bit faster (in my simulations at least)
freq.list.a.b <- c(freq.list.a, freq.list.b)
rowsum(freq.list.a.b, names(freq.list.a.b))

# approach 4 is the shortest, but certainly least memory-efficient, way and involves just using the frequency lists to create one big vector with all elements and tabulate that
table(                                      # generate a frequency table of what you get when you
   c(                                       # combine what you get when you
      rep(names(freq.list.a), freq.list.a), # repeat the words from corpus a as often as they appear there
      rep(names(freq.list.b), freq.list.b)  # repeat the words from corpus b as often as they appear there
   ) # end of c(...)
) # end of table(...)
# kind of cheating but possible with short vectors ...



# (11)  Using the approach from (10), let's see whether we can take two corpora such as text.1 and text.2 and quickly generate a frequency list revealing how often each word occurs in each corpus
text.1 <- c("this", "can", "be", "done", "very", "easily", "with", "R")
text.2 <- c("but", "other", "things", "may", "may", "be", "more", "difficult", "with", "R")
# That is, the desired result is this:
#            both.corpora
# both.texts  One Two
#   be          1   1
#   but         0   1
#   can         1   0
#   difficult   0   1
#   done        1   0
#   easily      1   0
#   may         0   2
#   more        0   1
#   other       0   1
#   R           1   1
#   things      0   1
#   this        1   0
#   very        1   0
#   with        1   1
both.texts <- c(text.1, text.2) # combine both corpora into one vector, then ...
# ... create an indicator variable that states for each token which corpus it's from:
both.corpora <- rep(c("One", "Two"),                   # put into both.corpora what you get when you repeat the words "One" and "Two"
                    c(length(text.1), length(text.2))) # as often as corpus 1 and corpus 2 have tokens
table(both.texts, both.corpora)                        # then cross-tabulate



# (12)  How do you find the words that
#       (a) occur in text.1 but not in text.2?
setdiff(text.1, text.2)
#       (b) occur in text.1 and text.2?
intersect(text.1, text.2)
#       (c) words that occur in at least one text?
union(text.1, text.2)



# (13)  Generate a frequency table of the vector qwe in which the frequencies are sorted according to each word's first occurrence in qwe.
qwe <- c("in", "a", "bed", "in", "the", "room", "is", "the")
table(qwe)[       # generate a table of qwe, but provide it in the order
   unique((qwe))] # in which words show up the first time (because unique ignores 2nd and more repetitions)



# (14)  Generate a vector new.types that returns for each word in qwe where it shows up the first time (we will use something like this for vocabulary growth curves)
new.types <- match(unique(qwe), qwe)
names(new.types) <- qwe[new.types]
new.types

# alternative with a function that was a recommendation for further study/exploration
match(qwe[!duplicated(qwe)], qwe)



### Exercise box 3.2: Handling data frames ########################################################
# (1)   Generate a data frame abc that contains the letters from "a" to "j" in the first column and the integers from 10 to 1 in the second column. Make sure the first column is called "LETTERS" and the second "NUMBERS".
LETTERS <- letters[1:10]; NUMBERS <- c(10:1)
(abc <- data.frame(LETTERS, NUMBERS))



# (2)   Load the text file <_qclwr2/_inputfiles/dat_dataframe-b.csv> into a data frame example such that the first row is recognized as containing the column names. Make the columns available for further processing.
example <- read.table(file.choose(), header=TRUE, sep="\t", comment.char="")
attach(example)



# (3)   Extract from this data frame
#       (a) the second and third column
example$RELATION; example$LENGTH # individually
example[,2:3] # jointly

#       (b) the third and fourth row
example[3:4,]



# (4)   Split the data frame example up according to the content of the second column (enter ?split at the R prompt for help).
split(example, RELATION) # this returns a list, a data structure you will learn about in the next section 3.5



# (5)   Reorder the data frame according to, first, the content of the second column (in reverse alphabetical order) and, second, the content of the third column (in ascending order).
order.index <- order(-rank(RELATION), LENGTH)
(example <- example[order.index,])



# (6) Save the changed data frame into a file <_qclwr2/_outputfiles/dat_dataframe-c.csv> such that you obtain a table that you could easily open in a spreadsheet software.
write.table(example, file="../_outputfiles/dat_dataframe-c.csv", sep="\t", eol="\n", row.names=FALSE, quote=FALSE)



# (7)   Generate the following data frame and call it EPP (for English personal pronouns) [...]
# PRONOUN   PERSON   NUMBER
# I         1        sg
# you       2        sg
# he        3        sg
# she       3        sg
# it        3        sg
# we        1        pl
# you       2        pl
# they      3        pl

PRONOUN <- c("I", "you", "he", "she", "it", "we", "you", "they")
PERSON <- c(1, 2, 3, 3, 3, 1, 2, 3)
NUMBER <- rep(c("sg", "pl"), c(5, 3))
(EPP <- data.frame(PRONOUN, PERSON, NUMBER))



# (8)   Extract from this data frame
#       (a) the value of the fourth row and the second column
EPP[4,2]

#       (b) the values of the third to fourth rows and the first to second columns
EPP[3:4, 1:2]

#       (c) the rows that have plural pronouns in them
EPP[NUMBER=="pl",] # one way
subset(EPP, NUMBER=="pl") # another way

#       (d) the rows with first and third person pronouns
EPP[(PERSON==1 | PERSON==3),]



# (9)   Make this vector of the frequencies with which the personal pronouns in EPP occurred in a small corpus the fourth column of EPP: I: 8426, you: 9462, he: 6394, she: 4234, it: 6040, we: 2305, you: 8078, they: 2998.
EPP[,"FREQS"] <- c(8426, 9462, 6394, 4234, 6040, 2305, 8078, 2998)
EPP


# (10)  Reorder the data frame according to, first, PERSON (ascending), second, NUMBER (alpabetically descending), and third, PRONOUN (alpabetically ascending).
(EPP <- EPP[order(PERSON, -rank(NUMBER), PRONOUN),])



# (11)  Save this data frame into a file called <_qclwr2/_outputfiles/dat_dataframe-d.csv> ; make this file as spreadsheet-software compatible as possible in terms of quotes, separators, and end-of-line characters.
write.table(EPP, file="../_outputfiles/dat_dataframe-d.csv", row.names=FALSE, quote=FALSE, sep="\t", eol="\n")



# (12)  The file <_qclwr2/_inputfiles/dat_dataframe-e.csv> contains data for the VERB into VERBing construction in the BNC (e.g., He forced him into speaking about it). For each instance of one such construction, the file contains the file where the instance was found (A06 in this case), the part-of-speech tag of the gerund (VVG in this case), the gerund (speaking), the lemma of the finite verb (force), and the lemma of the gerund (speak). Load this file into a data frame COV, display the first six rows of COV, and make the variables available. Correct the typo in line 3.
COV <- read.table(file.choose(), header=TRUE, sep="\t", comment.char="")
head(COV)
attach(COV)
COV$ING_LEMMA[3]<-"take" # or fix(COV)



# (13)  What
#       (a) is the quickest way of identifying the numbers of verb lemma types and -ing lemma types?
str(COV) # or
length(unique(VERB_LEMMA)); length(unique(ING_LEMMA))

#       (b) is the most frequent verb lemma?
sort(table(VERB_LEMMA)) # or
names(table(VERB_LEMMA))[which(table(VERB_LEMMA)== max(table(VERB_LEMMA)))] # or
summary(COV) # ;-)

#       (c) is the most frequent -ing lemma with this verb lemma?
x <- sort(table(ING_LEMMA[VERB_LEMMA=="force"]))
names(x[x==max(x)])



# (14)   Changing and saving COV
#       (a) Delete the column with the corpus files; the new table is to be called COV.2.
COV.2 <- COV[,2:5]

#       (b) Delete the rows with the four rarest tags; the new table is to be called COV.3.
(x <- sort(table(COV.2$TAG_ING))) # step 1: determine the rarest tags (here with eye-balling, we'll use better methods later)
COV.3 <- COV.2[            # make COV.3 the rows of COV.2 where
   !(COV.2$TAG_ING         # the elements in COV.2$TAG_ING are NOT (note the "!")
     %in%                  # in the set of
        head(names(x), 4)) # the names of the 4 rarest tags
   ,                       # no restrictions on columns
] # end of subsetting

#       (c) From COV.3, create a new table COV.4 which is sorted according to, first, the column VERB_LEMMA (ascending) and, second, the ING_LEMMA (descending).
order.index <- order(COV.3$VERB_LEMMA, -rank(COV.3$ING_LEMMA))
COV.4 <- COV.3[order.index,]

#       (d) Save the changed table into a text file <_qclwr2/_outputfiles/dat_dataframe-f.csv>; use tab stops as separators, newlines as line breaks, and make sure you don't have row numbers and no quotes.
write.table(COV.4, file="../_outputfiles/dat_dataframe-f.csv", sep="\t", eol="\n", quote=FALSE, row.names=FALSE)



### Exercise box 3.3: Conditional expressions #####################################################
# Write a few lines of code that 'educates' a user to enter 4 at the prompt. First, the script prompts the user to input the number 4 (use scan as mentioned in Section 3.2.2). Second, a conditional expression tests whether the user did enter 4 or not. If the user entered 4, the output should say "Good!" - if not, the output should say "Dummy, this is not 4. Enter 4 when you are prompted again!", and then the user is prompted to enter 4 again. Another conditional expression follows whether the user entered 4 this time. If yes, the output should read "Thanks, now it is right.", if not, the output should read "You will never learn ...".
aa <- scan(nmax=1, quiet=TRUE)
if (aa==4) {
   cat("Good!\n")
} else {
   cat("Dummy, this is not 4. Enter 4 when you are prompted again!\n")
   aa <- scan(nmax=1, quiet=TRUE)
   if (aa==4) {
      cat("Thanks, now it is right.\n")
   } else {
      cat("You will never learn \n")
   }
}



### Exercise box 3.4: Loops #######################################################################
# Define a vector letts with the letters from "d" to "m". Write a loop that prints each letter and at which position in letts it can be found.
letts <- letters[4:13]
for (counter in seq(letts)) {
   cat("This is letter number",
       counter,
       "in letts, namely a(n)",
       letts[counter],
       ".\n")
}

# excursus: check out the function formatC, which you can see in action here:
for (counter in seq(letts)) {
   cat("This is letter number",
       formatC(counter, width=2, flag=0),
       "in letts, namely a(n)",
       letts[counter],
       ".\n")
}
# what's that good for you ask? IT can be useful in applications where you generate, say, thousands of output files into a directory and you want them numbered from 0001 to 2000, and you want them numbered that way because of then your operating system will sort them properly as opposed to sorting 1, 10, 100, ...



### Exercise box 3.5: tapply ######################################################################
# (1)   Load the text file <_qclwr2/_inputfiles/dat_dataframe-b.csv> into a data frame example such that the first row is recognized as containing the column names. Make the columns available for further processing. Use tapply to determine
#       (a) the summed length of objects and subjects;
example <- read.table(file.choose(), header=TRUE, sep="\t", comment.char="") # <_qclwr2/_inputfiles/dat_dataframe-b.csv>
summary(example)
attach(example)
tapply(LENGTH, RELATION, sum)

#       (b) the average length of definite and indefinite subjects and objects.
tapply(LENGTH, list(RELATION, DEFINITENESS), mean)



# (2)   Load the text file <_qclwr2/_outputfiles/dat_dataframe-d.csv> into the data frame EPP again such that the first row is recognized as containing the column names. Make the columns available for further processing. Compute the sum of all occurrences of singular and plural in the corpus.
EPP <- read.table(file.choose(), header=TRUE, sep="\t", comment.char="") # <_qclwr2/_outputfiles/dat_dataframe-d.csv>
EPP
attach(EPP)
tapply(FREQS, NUMBER, sum)



### Exercise box 3.6: A few regular expressions ###################################################
# (1)   Imagine you have a character vector qwe looking like this " I   am  not very       hungry.   " How would you clean up qwe such that you remove sequences of more than one space as well as leading and trailing spaces?
(qwe <- " I   am  not very       hungry.   ")
qwe.2 <- gsub(" {1,}", " ", qwe, perl=TRUE) # or
qwe.2 <- gsub(" +",    " ", qwe, perl=TRUE) # with " +" as search expression, or
qwe.2 <- gsub(" {2,}", " ", qwe, perl=TRUE) # better because it involves fewer replacements

qwe.3 <- gsub("(^ | $)", "", qwe.2, perl=TRUE) # deleting leading or trailing spaces
qwe.3
# later, we will write a function called whitespace that automates the above and more



# (2)   Imagine you have a character vector qwe which contains many expression such as "in 1987" and "as of 2002" etc., i.e. numbers denoting years. How would you replace all occurrences of four digits that start with either "19" or "20" by "YEAR"?
(qwe <- "These 1000 accidents happened in 1905, not 2006.")
gsub("(19|20)[0123456789][0123456789]", "YEAR", qwe, perl=TRUE) # or
gsub("(19|20)[0-9][0-9]",               "YEAR", qwe, perl=TRUE) # or
gsub("(19|20)[0-9]{2}",                 "YEAR", qwe, perl=TRUE)

# You will also learn about this more elegant approach presently.
gsub("(19|20)\\d\\d", "YEAR", qwe, perl=TRUE) # or
gsub("(19|20)\\d{2}", "YEAR", qwe, perl=TRUE)



# (3)   How would you match the different spelling variants that people use for
#       (a) the word "separate"?
# grep("sep[ae]?r[ae]te", ..., perl=TRUE)

#       (b) the British and American spellings of the word "analyze"?
# grep("analy[sz]e", ..., perl=TRUE)



### Exercise box 3.7: A few more regular expressions ##############################################
# (1)   Imagine you have this vector txt:
txt<-"<w VVB>Introduce <w NP0>Brenda <w PNQ>who<w VBZ>'s <w VVG>going <w TO0>to <w VVI>speak <w PRP>to <w PNP>us <w AVP-PRP>on <w VVB>Make <w VDI>do <w CJC> and <w VVB>Mend <w CJC>and <w PNP>she<w VHZ>'s <w VVN>asked <w PNP>me <w TO0>to <w VVI>say <w CJT>that<c PUN>."

#       How would you use regular expressions to delete everything but content words and their tags where content words are defined as words that are tagged as adjectives (i.e., whose tag begins with "<w AJ"), adverbs ("<w AV"), common but not proper nouns ("<w N" but not "<w NP"), or verbs "<w V" or whose first part of their portmanteau tag is tagged that way?
gsub("<(?!w ((AJ.|AV.|N[^P].|V..)(-...)?)).*?>[^<]*", "", txt, perl=TRUE)
# note how, for instance, "<w NP0>Brenda ", "<w PNQ>who", "<w CJT>that", and "<c PUN>." are gone.



# (2)   Imagine you have a this vector txt. How would you use regular expressions to delete the letters before the closing tag "</B>"?
txt <- "Yes, this <B>is</B> bold type."
gsub("\\w*(?=</B>)", "",    txt, perl=TRUE) # deleting with lookaround
gsub("\\w*(</B>)",   "\\1", txt, perl=TRUE) # deleting, but putting back in with backreferencing



# (3)   We return to the rhyming question. You read how to match two adjacent word that end in the same character sequences. Imagine you have the following vector txt:
txt <- c("This is not my dog.", "This not is my dog.")

#       This is the regex we used to tag the is-is sequence in the first element of txt.
gsub("(\\w{2,}?)(\\W+\\w*?)\\1\\b",
     "\\1<r>\\2\\1<r>",
     txt,
     perl=TRUE)

#       Now, how do you also get the is's in the second element of txt tagged where they are not adjacent?
gsub("(\\w+?)\\b(.+\\w*?)\\1\\b",
     "\\1<r>\\2\\1<r>",
     txt,
     perl=TRUE)



# (4)   How do you use lookaround with exact.matches.2 to get only the words from the following vector txt?
txt<-"<w DPS>my <w NN1>mum <w CJC>and <w DPS>my <w NN1>aunt <w VVD>went <w PRP>into <w NN1>service"
exact.matches.2("(?<=>).+?(?= ?<)", txt)[[1]]

exact.matches.2("(?x)       # set free-spacing
                (?<=>)      # look to the left and see a >
                .+?         # then match stuff
                (?=\\s?<)", # but only when, consuming stuff, you can look the right and see maybe a space and then a <
                txt)[[1]]   # in txt, but return only the exact matches (which are in component [[1]] of the output)
# note how the free-spacing mode made it necessary to change the " " after ?= to a \\s



### Exercise box 3.8: Unicode #####################################################################
# (1)   Load the file with the Russian text in Cyrillic again and generate an alphabetically-sorted frequency list of all words that begin with a capital Cyrillic character.
rm(list=ls(all=TRUE)) # clear memory
corpus.file <- readLines(con <- file(file.choose(), encoding="UTF-8"), warn=FALSE); close(con) # <_qclwr2/_inputfiles/corp_utf8_cyrillic.txt>
tail(corpus.file)

(russ.char.no <- "[^\u0401\u0410-\u044F\u0451]") # all other characters

words <- unlist( # put into words the unlisted list you get when you
   strsplit(       # strplit
      corpus.file, # the vector corpus.file
      paste0(          # at merged (without a separator) occurrences of
         russ.char.no, # non Cyrillic characters
         "+"           # specifically, 1 or more
      ),               # end of paste0
      perl=TRUE        # using Perl-compatible regular expressions
   ) # end of strsplit(...)
) # end of unlist(...)
words <- words[nzchar(words)] # remove empty character strings

freq.table <- table(words) # generate an alphabetically-sorted frequency list of all words
freq.table <- freq.table[  # create a new version of freq.table from the old version of freq.table
   grepl(                       # namely retain only those
      "^[\u0410-\u042F\u0401]", # where you find at the beginning a capital Cyrillic character
      names(freq.table),        # in the names of freq.table, i.e. all word types in the text
      perl=TRUE)                # using Perl-compatible regular expressions
] # end of [...]
freq.table                 # look at the output



# (2)   Generate a frequency list of the words in the Russian text that is sorted by (i) the length of the words (in ascending order) and by (ii) the frequency of the tokens.
freq.table <- table(words) # generate an alphabetically-sorted frequency list of all words
type.lengths <- nchar(names(freq.table)) # find the lengths of the word types
order.index <- order(type.lengths, freq.table) # create an order index, 1st by lengths, then by frequency
freq.table <- freq.table[order.index] # re-order the frequency list



# (3)   Generate a concordance of all the words that contain an "о" ("\u043e").
# The search expression is any Cyrillic character 0 or more times (greedy), followed by the о, followed by any Cyrillic character 0 or more times (greedy):
exact.matches.2("[\u0401\u0410-\u044F\u0451]*\u043e[\u0401\u0410-\u044F\u0451]*",
                corpus.file)[[4]]



### Exercise box 3.9: XML #########################################################################
# (1)   This example brings together a few things and will prepare you a bit for one of the more complex ones in Chapter 5. The task is to find all unique elements in the British National Corpus XML data that are annotated as multi-word units (MWUs), i.e. like this: "<mw c5="AV0"><w c5="NN1" hw="sort" pos="SUBST">sort </w><w c5="PRF" hw="of" pos="PREP">of </w></mw>". Specifically, you want to find all multi-word forms (i.e., here, "sort of") and their tags (i.e., here, "AV0").
# If you have access to the BNC XML, use it for this, and use one of R's XML libraries; otherwise, use the files in <_qclwr2/_inputfiles/corp_bnc_xml-sample> (for which you cannot use the XML libraries because the files are not well-formed XML). To keep matters simple here, we'll just work with the first corpus file from each directory, the real BNC or the sampled one.

rm(list=ls(all=TRUE)) # clear memory
source(file.choose()) # source the function exact.matches.2 from <_qclwr2/scripts/>

corpus.path <- dir(    # create a vector corpus.files with the content of
   rchoose.dir(),      # a user-defined directory (with tcltk: tk_choose.dir()) and
   recursive=TRUE,     # all its (non-hidden) files as well as all the (non-hidden) files in all its subfolders
   full.names=TRUE)[1] # and retain all full paths, but ultimately keep only the first one



# this is the code to use with the proper BNC XML and the XML package
library(XML) # load the library XML
curr.corpus.file <- xmlInternalTreeParse(scan(corpus.path, what=character(), sep="\n", quiet=TRUE))

curr.mwus.words <- xpathSApply(curr.corpus.file, # apply XPath syntax to elements of curr.corpus.file
                               "//mw",           # namely the multi-word units below sentences
                               xmlValue)         # then return the data values (i.e., the multi-word units)
curr.mwus.words <- gsub("(^ +| +$)",     # homogenize by replacing leading/trailing spaces
                        "",              # with nothing
                        curr.mwus.words, # in curr.mwus.words
                        perl=TRUE)       # using Perl-compatible regular expressions

curr.mwus.tags <- xpathSApply(curr.corpus.file, # apply XPath syntax to elements of curr.corpus.file
                              "//mw",           # namely the multi-word units below sentences
                              xmlGetAttr,       # then extract the attribute
                              "c5")             # called "c5"

sort(unique(tolower(paste(curr.mwus.words, curr.mwus.tags, sep="_"))))



# this is the code to use with the proper BNC XML and the xml2 package
library(xml2)
curr.corpus.file <- read_xml(corpus.path)

curr.mwus.words <- xml_find_all(curr.corpus.file, ".//mw") # find all MWU elements

curr.mwus.tags <- exact.matches.2( # put into curr.mwus.tags what you retrieve when you
   "(?<=<mw c5=\")[^\"]+",         # find the C5 tag of the multi-word unit
   as.character(curr.mwus.words),  # in the character vector version of curr.mwus.words
   gen.conc.output=FALSE)[[1]]     # generate no concordance, get only exact matches

curr.mwus.words <- gsub("\\s*<.*?>", " ",              # replace all tags & preceding whitespace by 1 space
                        as.character(curr.mwus.words), # in the character vector version of curr.mwus.words
                        perl=TRUE)                     # using Perl-compatible regular expressions
curr.mwus.words <- gsub("(^ +| +$)",     # homogenize by replacing leading/trailing spaces
                        "",              # with nothing
                        curr.mwus.words, # in curr.mwus.words
                        perl=TRUE)       # using Perl-compatible regular expressions
curr.mwus.words <- gsub(" {2,}",         # replacing 2+ spaces
                        " ",             # with 1
                        curr.mwus.words, # in curr.mwus.words
                        perl=TRUE)       # using Perl-compatible regular expressions

sort(unique(tolower(paste(curr.mwus.words, curr.mwus.tags, sep="_"))))



# this is the code to use with <_qclwr2/_inputfiles/corp_bnc_xml-sample> using flat text processing
curr.corpus.file <- tolower( # make curr.corpus.file the lower-case version of
   scan(                     # what you get when you load
      corpus.path, what=character(), sep="\n", # the current path as a line-break-separated chr vector
      quote="", comment.char="", quiet=TRUE))  # no quotes, comment.chars, and no line number feedback

curr.mwus.tokens <-
   exact.matches.2("<mw .*?</mw>",             # find the mw tags with what they tag
                   curr.corpus.file,           # in curr.corpus.file
                   gen.conc.output=FALSE)[[1]] # generate no concordance, get only exact matches

curr.mwus.tags <-
   exact.matches.2("(?<=<mw c5=\")[^\"]+",     # find the C5 tag of the multi-word unit
                   curr.mwus.tokens,           # in curr.mwus.tokens
                   gen.conc.output=FALSE)[[1]] # generate no concordance, get only exact matches

curr.mwus.words <- gsub("<.*?>",               # get the words by replacing all tagged stuff
                        "",                    # with nothing
                        curr.mwus.tokens,      # in curr.mwus.tokens
                        perl=TRUE)             # using Perl-compatible regular expressions
curr.mwus.words <- gsub("(^ +| +$)",     # homogenize by replacing leading/trailing spaces
                        "",              # with nothing
                        curr.mwus.words, # in curr.mwus.words
                        perl=TRUE)       # using Perl-compatible regular expressions

sort(unique(tolower(paste(curr.mwus.words, curr.mwus.tags, sep="_"))))



### Exercise box 3.10: Writing your own functions ... #############################################
# (1)   Above, we looked at code that lists all characters attested in a character vector; we did it like this:
txt.1 <- c(" This     is    one     example sentence.  ", "  This    is \t another \t one.  ", "And yet     \t another    \t   one.   ")
table(unlist(strsplit(txt.1, ""))) # maybe also with sort

# Write a function character.lister that takes as input a one-element character vector and returns a list with two components:
# - one component with an alphabetically-sorted frequency list of all characters
# - another component with a frequency-sorted frequency list of all characters

character.lister <- function (some.char.vector) {
   temp <- table(unlist(strsplit(some.char.vector, "")))
   output <- list("AlphabeticalOrder"=temp,
                  "FrequencyOrder"=sort(temp, decreasing=TRUE))
   return(output)
}
character.lister(txt.1)



# (2)   Above, we looked at code that generated a comparative frequency table for two corpora, which worked like this:
text.1 <- c("this", "can", "be", "done", "very", "easily", "with", "R")
text.2 <- c("but", "other", "things", "may", "may", "be", "more", "difficult", "with", "R")
both.texts <- c(text.1, text.2)
both.corpora <- rep(c("One", "Two"), c(length(text.1), length(text.2)))
table(both.texts, both.corpora)

# Now, try to write a function corpus.comparer that does the above. If you want to go beyond this simple assignment, make the function
# - be able to handle however many corpora it is given. Hint: that may be easiest if the argument to corpus.comparer is a list whose named elements are all the corpora the user wants to compare.
# - sorts the table such that the largest row and column sums are at the top and the left of the table.
# This could be the way it looks:
rm(list=ls(all=TRUE)); set.seed(1)
text.1 <- sample(letters[1:10], 200, replace=TRUE)
text.2 <- sample(letters[1:10], 250, replace=TRUE)
text.3 <- sample(letters[1:10], 300, replace=TRUE)

all.data <- list(CORPUS1=text.1, CORPUS2=text.2)
corpus.comparer(all.data)
#      CORPORA
# WORDS CORPUS2 CORPUS1 Sum
#   e        31      24  55
#   d        31      23  54
#   b        36      17  53
#   c        25      22  47
#   h        20      27  47
#   j        31      15  46
#   g        18      21  39
#   i        19      20  39
#   f        19      19  38
#   a        20      12  32
#   Sum     250     200 450

all.data <- list(CORPUS1=text.1, CORPUS2=text.2, CORPUS3=text.3)
corpus.comparer(all.data)
#      CORPORA
# WORDS CORPUS3 CORPUS2 CORPUS1 Sum
#   d        35      31      23  89
#   e        32      31      24  87
#   b        28      36      17  81
#   h        32      20      27  79
#   j        33      31      15  79
#   g        33      18      21  72
#   f        32      19      19  70
#   c        19      25      22  66
#   i        26      19      20  65
#   a        30      20      12  62
#   Sum     300     250     200 750

corpus.comparer <- function(list.of.corpora) {
   WORDS <- as.character(unlist(list.of.corpora))
   CORPORA <- rep(names(list.of.corpora),
                  sapply(list.of.corpora, length))
   output <- table(WORDS, CORPORA)
   output <- addmargins(
      output[
         order(-rowSums(output)),
         order(-colSums(output))
         ]
   )
   return(output)
}



# (3) Pretend that the function grep did not have an argument invert=TRUE and write a function grep.invert that returns the same as grep(pattern, x, invert=TRUE):
txt <- c("This is a first example sentence.", "And this is a second example sentence.")
grep("second", txt, invert=TRUE) # [1] "This is a first example sentence."
grep.invert("second", txt)       # [1] "This is a first example sentence."

grep.invert <- function(search.string, corpus.vector) { # define a function grep.invert, which
   return(                       # returns
      corpus.vector[             # those parts of corpus.vector,
         !                       # which are the negation/opposite of
            grepl(search.string, # the logical values when looking for the search string
                  corpus.vector) # in the corpus.vector
      ]                          # end of subsetting
   )                             # end of return(...)
} # end of function definition
grep.invert("second", txt)
