library(tidyverse)
library(stringr)

#Gries Ch.3, Scratch notes for lecture

##Explain the difference between R and R Studio
##Explain the difference between a console and a script

##Show that you can do basic math in R

1+3 #You can do addition
3*3 #You can do multiplication
4/2 #You can do division
3^2 #You can do exponents
(3+7)^2 #You can do embedding

##Show what a function is
##A function always has the structure function(x)
##with x being what you want to change

sqrt(4) #You can square root with a function
log(100,10) #You can log with a function, here we log with base 10
sum(1,2,4)
?mean
mean(1,2,4) #Doesn't work with mean(), I'll explain why in a second


##Show that you can assign numbers to objects
pi #This is an object in R
##You can create new ones
m <- 4
m + 3 
##You can overwrite objects
m <- m+4
m+3

##You can make a list of things, this is called a *vector*
some_numbers <- c(3,4,5,1,9) ##You do this with c(), which stands for combine
some_numbers *2 ##You can do math on your vector
##Vector is a one-dimensional ordered sequence of elements
##Vectors can be *numerics* or *characters*
parent_names <- c("Ross", "Barbara")
sibling_names <- c("Jacob", "Alyson")
my_name <- "Adam"

##You can combine vectors
tallman_names <- c(parent_names, sibling_names, my_name)
tallman_names

##You can refer to properties of the vector
tallman_names[1] #Just the first name
tallman_names[1:3] #The first three names

ages_years <- c(60, 58, 34, 31, 36)

ages_months <- ages_years * 12

mean(ages_years) #You can get the average age, note mean() only takes vectors not individual elements

##R gives you error messages when you ask it to do things it does not understand
tallman_names*2

##You can load vectors into R
##Create a text file
##Load it into R

text_01 <- scan(file.choose(), what=character(), sep="\n") ##This reads it in line by line
length(text_01)

text_02 <- scan(file.choose(), what=character(), sep=" ")
length(text_02)

##Write something in cyrillic and read it in (perhaps not relevant for my students)
##Read Spector's data manipulation with R

letters_01 <- c("a", "b", "c", "d", "e", "f", "g", "h")
head(letters_01,2)
tail(letters_01, 2)

##You can subset in a vector
letters_01[5] #With element 5
letters_01[-5] #Without element 5

some_integers <- c(10:1)

##R has a bunch of logical operators, the output is always true or false

some_integers ==2 #which elements are equal to 2
some_integers !=2 #which elements are not equal to 2
some_integers <= 4 #which elements are greater than 2

##You can target specific elements with the function which()

which(some_integers==3) #You can refer to the position of an element

##You can ask whether specific elements are in vectors
"I" %in% text_02

##There's an important function match that asks how two vectors might match one another, one by one

x<- c(10:1)
y <- c(2,4,5)
match(x,y) ##Tells you the index of y which matches each of the numbers in x

match(y,x) ##Tells you the index of x that matches each of the numbers in y

##How could match be useful - if I wanted to find sentences where just two glosses occur

morphs_to_be_found <- c("pi-", "-ma")
morphs <- c("wada", "ema", "eami", "pi-", "ti", "-ma", "a", "-lelajai")
match(morphs, morphs_to_be_found)
##In an actual case you would have to group by sentence

##How can I show this is useful for students
morphs <- c("should","have","has","been")
sentence_1 <- c("He","really", "should", "not", "have", "come")
sentence_2 <- c("John", "has", "done", "the", "homework")
match(sentence_1, morphs)

simulate_convergence <- function(base, tests, positions){ 
  left <- as.integer(runif(tests, 1, base))
  right <- as.integer(left + runif(tests, base, positions))
  simulated_spans <- table(paste(left, right)) 
  simulated_spans <- as.data.frame(simulated_spans)
  colnames(simulated_spans) <- c("Span", "Freq")
  converging_spans <- simulated_spans %>%
    filter(., Freq>=2)
  print(converging_spans)
  }

simulate_convergence(5,20,10)

simulated_data_list <- replicate(n=100, expr=simulate_convergence(5,20,10), simplify = FALSE)

simulated_data_df <- bind_rows(simulated_data_list, .id="trial_number")

simulated_data_df$Span <- as.character(simulated_data_df$Span)


for (i in simulated_data_df$Span[1:length(simulated_data_df$Span)]){
  str_replace(i," ","-")
}

head(simulated_data_df)

length(simulated_data_df)

##set diff, tells you what numbers are different

setdiff(x,y)
setdiff(y,x)
setdiff(sentence_1, morphs)

##intersect tells you what numbers are the same
intersect(x,y)
intersect(sentence_2, morphs)
##union() provides elements that occur at least once in the combination of the two vectors

##Saving vectors, you can save vectors with cat()

cat(sentence_1, file=file.choose(), append = FALSE)

##Vectors are used when you have an input
##But Dataframes are what you need prior to statistical analysis
##Dataframes are vectors lined up beside each other vertically (long format)
##Usually you would figure out a way to turn your corpus input into a dataframe output

pos <- c("adj", "adv", "n", "conj", "prep")
tokenfreq <- c(421,337,1411,458,455)
typefreq <- c(271, 102, 735, 16, 37)
class<- c("open", "open", "open", "closed", "closed")

df_1 <- data.frame(pos, tokenfreq, typefreq, class)


##You can refer back to parts of the dataframe with $
df_1$pos

##Dataframes are types of lists
##Dataframes are types of lists where all vectors that all have the same length
##60-65 has a discussion of lists, maybe go back to this
##It was not obvious how its going to be useful for corpus linguistics

##ifelse statements, whne to use
###include a corpus file in your analysis only if the corpus header reveals it is a file containing spoken language
###search a line of a corpus file only if it belongs to a particular utterance
###want to use one search expression if a file has SGML annotation and another if it has XML annotation etc.

distributions <- NULL
for (i in 1:3){
  for (j in 2:4){
  distributions_01 <- (rnorm(10, mean=i, sd=j))
  distributions <- cbind(distributions_01, distributions)
  }
}

##tapply is a useful function
tapply(df_1$tokenfreq, df_1$class, sum) #for every level of CLASS, determine the sum of the values in tokenfrequency
##*I should teach them tapply - it seems like a useful function

##*Character/String Processing (in general Section 3.7 seems like what is useful)
##*note to self: BNC uses SGML annotation

##Add this after regular expressions section
practice_string <- c("<s id=1><w pos='pronoun'>I</w><w pos='v'>was</w><w pos='adj'>tired</w></s>")

get_words <-function(string){
  a <- str_match_all(string, "<w(.*?)</w>")
  a1 <- as.data.frame(a)
  words <- str_remove(a1$X2, ".*>")
}

get_words(practice_string)

get_pos <- function(string){
  b <- str_match_all(string, "pos='(.*?)'")
  b1 <- as.data.frame(b) 
  tags <- b1$X2
}

a <- str_match_all(practice_string, "pos='(.*?)'")
a
get_pos(practice_string)

get_sid <- function(string){
  c <- str_match_all(string, "id=[0-10]")
  c1 <- as.data.frame(c)
  sentence_id <- str_remove(c1[1,], "id=")
}

df <- NULL

practice_parse <- function(string){
    a <- str_match_all(string, "<w(.*?)</w>")
    a1 <- as.data.frame(a)
    words <- str_remove(a1$X2, ".*>")
    b <- str_match_all(string, "pos='(.*?)'")
    b1 <- as.data.frame(b) 
    pos <- b1$X2
    c <- str_match_all(string, "id=[0-10]")
    sentence_id <- str_remove(c, "id=")
    id_n <- str_count(practice_string, "</w>")
    sentence_id <- rep(sentence_id, id_n)
    df <- cbind(words, pos, sentence_id)
    print(df)
}
practice_string_parsed <- practice_parse(practice_string)
practice_string_parsed

nzchar(practice_string)

##Using grep

txt <- c("This is a first example sentence", "And this is a second example sentence")

grep("second", txt) #find second in the txt

grep("eco", txt, value=TRUE) #you get the actual sentence with this one

grepl("first", txt) #This asks whether the text is found in that location
##This one is useful for a getting dispersions
as.integer(grepl("first", txt))
##gregexpr is a really sophisticated one

gregexpr("e", txt)

ges.output <- gregexpr("e", txt)

##Regular expressions
txt <- c("This is a first example sentence", "And this is a second example sentence")
txt <- c(txt, "And I added another sentence")
txt <- cat(txt, sep="\n")

##grep() allows you to put in a regular expression
grep("^t", txt, ignore.case=TRUE, perl = TRUE, value=TRUE) ##The caret finds the beginning of the sentence
grep("^a", txt, ignore.case=TRUE, perl = TRUE, value=TRUE) ##The caret finds the beginning of the sentence

grep("<w", practice_string, ignore.case=TRUE, perl = TRUE, value=TRUE) ##The caret finds the beginning of the sentence

gsub("^t", "_T_", txt, ignore.case=TRUE, perl=TRUE)

##'.' can stand in for any character
##If you want to have a period, you can use \\.
##This is called an escape character
##$ Marks the end of a character string

color.spellings <- c("colour", "color")

grep("colou{0,1}r", color.spellings, perl=TRUE, value=TRUE)

##You can replace with gsub
txt2 <- gsub("\\b", "<w>", txt, perl=TRUE)

##Look-ahead function uses (?=...): ?! is the negative version
##Look-behind function uses (?<=): ?<! is the negative version
##. means any character
##* means any number of times

sentences <- c("I gave Jim the apple", "I gave the apple to Jim")
grep("gave.*(?=.to)", sentences, perl=TRUE) ##"gave" then any character any number of times and then "to"

##Merging and splitting strings
##while gsub() might be the starting point to automatically tag a text
##strsplit is the starting point to parse one

tagtext <- "<s id=1><w pos='pronoun'>I</w><w pos='v'>was</w><w pos='adj'>tired</w></s>"

tagtext <- "<w pos='noun'><m>I</m></w><w pos='aux'><m>was</m></w><w pos='v'><m>study</m><m>-ing</m></w> <w pos='n'><m>corpus</m></w> <w pos='noun'><m>linguistic</m> <m>-s</m></w>"

tagtext2 <- unlist(strsplit(tagtext,"</w>", perl=TRUE))
tagtext3 <- str_remove(tagtext2,"<w pos='[nounauxverb]*'>")
tagtext3

#Now let's assign each of the words an index
word_index <- 1:length(tagtext3)

tagtext4 <- as.data.frame(cbind(tagtext3, word_index))
tagtext4

tagtext4 %>%
  mutate(morph = strsplit(as.character(tagtext3), "</m>")) %>%
  unnest(morph) %>%
  filter(morph != "") %>%
  select(morph, tagtext3:word_index)



tagtext3$morphs<- unlist(strsplit(tagtext3$tagtext2,"</m>", perl=TRUE)) 






tagtext4 <- str_remove(tagtext4, "<m>")
tagtext4[! tagtext4 %in% " "]
morph_index <- 1:length(tagtext4)
tagtext5 <- as.data.frame(cbind(tagtext4, morph_index))



