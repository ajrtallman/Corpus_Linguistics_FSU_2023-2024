##Reading XML in R notes

#This is what you normally need
#install.packages("XML")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("devtools")
#library(devtools)
#install_github("dalejbarr/elan")

#Its not clear that you can read XML files directly into R
#I will need to learn more about XML in order to do this or the XML package in R 
#There is a package available just for reading eaf files
#https://github.com/dalejbarr/elan

#For general data manipulation
library(tidyverse)

#This is the library you need
library(elan)

setwd("/Users/Adan Tallman/Desktop/xml")

#This is data exported from FLEX into ELAN
annotations <- efileAnnotations("ara_2613_glossed_2.eaf")
view(annotations)

write.csv(annotations, "annotations.csv")

# parse the xml tree
doc <- elanTree("ara_2613_glossed_2.eaf") # name of your .eaf file goes here



# list of all the tiers and their attributes
tiers <- readTierList(doc)


# read in "alignable" annotations
# (associated with time codes)
ann.ali <- readAnnotations(doc)

#write.csv(ann.ali, file="ann.ali.csv")

#glimpse(ann.ali)

#view(ann.ali)

#head(ann.ali)


# read in "reference" annotations
# (NOT associated with time codes)
ann.ref <- readAnnotations(doc, "REF") 
write.csv(ann.ref, file="ann.ref.csv")

# which of the tiers have time codes associated with them?
tiers %>%
  inner_join(ann.ali, by="TIER_ID") %>% # join tiers to ann.ali
  select(TIER_ID) %>% # only keep the TIER_ID column
  unique # get rid of duplicates

# ... and how many annotations are there for each?
tiers %>%
  inner_join(ann.ali, by="TIER_ID") %>%
  group_by(TIER_ID) %>%  # form groups based on TIER_ID
  summarize(n=n()) # count how many in each group

# who spent the most time speaking?
tiers %>%
  filter(!is.na(PARTICIPANT)) %>% # PARTICIPANT field cannot be NA
  inner_join(ann.ali, by="TIER_ID") %>% 
  mutate(Duration=t1-t0) %>%  # calculate duration of each annotation
  group_by(PARTICIPANT) %>%
  summarize(nPhrases=n(), # count phrases
            secs=sum(Duration)/1000) # sum Duration & convert to secs

# and what was the speaking rate?
# 1. calculate duration of each annotated segment

segdur <- tiers %>%
  filter(!is.na(PARTICIPANT)) %>%
  inner_join(ann.ali, by="TIER_ID") %>%
  mutate(Duration=(t1-t0)/1000) %>%
  select(ANNOTATION_ID, PARTICIPANT, Duration)

plot(density(segdur$Duration))


# 2. pull out the morphs, then link to segdur

view(ann.ref)


# now calculate speech rate
words <- ann.ref %>%
  # use a regular expression to select the TIER_ID we want
  filter(.,TIER_ID=="A_word-txt-es" | TIER_ID =="B_word-txt-es") %>% 
  select(-ANNOTATION_ID, # drop it
         ANNOTATION_ID=ANNOTATION_REF, # replace for join
         Word=VALUE) # just rename the annotation field

view(words)

words %>%
  group_by(ANNOTATION_ID) %>% # each ANNOTATION_ID is one turn
  summarize(nWords=n()) %>% # count words
  inner_join(segdur, by="ANNOTATION_ID") %>% # join with durations
  select(-ANNOTATION_ID) %>% # get rid of this field
  mutate(wps=nWords/Duration) %>% # rate=words/duration
  group_by(PARTICIPANT) %>% 
  summarize(meanWPS=mean(wps)) %>%
  arrange(desc(meanWPS)) # descending order (fastest spkr first)

# what words were used, and with what frequency?
words %>%
  filter(!(Word %in% c(",", "?", "’", "...", "-", "[?]", "NW:", "MM:"))) %>% # lose code symbols
  group_by(Word) %>%
  summarize(n=n()) %>%
  filter(n>1) %>% # git rid of words that only occurred once
  arrange(desc(n)) # print in descending order
view(words)

##Keeping words with their annotation tier
words2 <- ann.ref %>%
  # use a regular expression to select the TIER_ID we want
  filter(.,TIER_ID=="A_word-txt-es" | TIER_ID =="B_word-txt-es") %>% 
  select(TIER_ID, ANNOTATION_ID, ANNOTATION_REF,
         Word=VALUE) # just rename the annotation field

# now calculate speech rate
morphs <- ann.ref %>%
  # use a regular expression to select the TIER_ID we want
  filter(.,TIER_ID=="A_morph-txt-es" | TIER_ID =="B_morph-txt-es") %>%
  select(TIER_ID, ANNOTATION_ID, ANNOTATION_REF, Morphs = VALUE)
morphs %>%
  filter(!(Morphs %in% c(",", "?", "’", "...", "-", "[?]", "NW:", "MM:"))) %>% # lose code symbols
  group_by(Morphs) %>%
  summarize(n=n()) %>%
  filter(n>1) %>% # git rid of words that only occurred once
  arrange(desc(n)) # print in descending order

words2$ANNOTATION_ID_WORDS <- words2$ANNOTATION_ID 
words2$ANNOTATION_ID_PHRASE <- words2$ANNOTATION_REF
words2$TIER_ID_WORDS <- words2$TIER_ID
words$LEVEL <- "WORD"
morphs$ANNOTATION_ID_WORDS <- morphs$ANNOTATION_REF
morphs$ANNOTATION_ID_MORPHS <- morphs$ANNOTATION_ID
morphs$TIER_ID_MORPHS <- morphs$TIER_ID
morphs$LEVEL <- "MORPH"
words2 <- words2 %>% select(-ANNOTATION_ID, -ANNOTATION_REF, -TIER_ID)
morphs <- morphs %>% select(-ANNOTATION_ID, -ANNOTATION_REF, -TIER_ID)
wordsandmorphs <- merge(words2, morphs, by = "ANNOTATION_ID_WORDS")
view(wordsandmorphs)

counts <- count(wordsandmorphs, ANNOTATION_ID_WORDS)

mean(counts$n)


phrases <- ann.ali %>%
 #use a regular expression to select the TIER_ID we want
  filter(.,TIER_ID=="A_phrase-segnum-es" | TIER_ID =="B_phrase-segnum-es") %>%
  select(TIER_ID, ANNOTATION_ID, t0.id, t0, t1.id, t1,
         Phrase=VALUE) # just rename the annotation field
head(phrases)
phrases$ANNOTATION_ID_PHRASE <- phrases$ANNOTATION_ID
phrases$TIER_ID_PHRASE <- phrases$TIER_ID
phrases <- phrases %>% select(-ANNOTATION_ID, -TIER_ID)

morphsphrases <- merge(wordsandmorphs, phrases, by = "ANNOTATION_ID_PHRASE")
view(morphsphrases)
head(morphsphrases)

counts_words <- morphsphrases %>%
  distinct(ANNOTATION_ID_WORDS, .keep_all = TRUE) %>%
  count(., ANNOTATION_ID_PHRASE)

counts_words$counts <- "Words per utterance" 

counts_morphs <- morphsphrases %>%
  count(., ANNOTATION_ID_PHRASE)

counts_morphs$counts <- "Morphs per utterance"

d <- cbind(counts_morphs, counts_words)

df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5)))
)

