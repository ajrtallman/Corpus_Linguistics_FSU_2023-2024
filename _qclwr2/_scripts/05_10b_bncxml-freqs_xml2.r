rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs)  # load the library for tk_choose_files
library(XML)    # load the library XML
library(xml2)   # load the library xml2
options(warn=1) # make R show any warnings as soon as they arise

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
source(file.choose()) # source the function exact.matches.2

# define the directory where the BNC files are located and get the file names
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)
dir.create("../_outputfiles/05_10_freqoutput_xml2") # create a subdirectory for the frequency list files

for(i in corpus.files) { # access each file
   cat( # output a progress report to the screen
      basename(i), "\n"
      ) # the file that will be loaded on this iteration, followed by a line break

   # with xml2
   current.corpus.file <- read_xml(i)

   mw.units.1 <- xml_find_all(current.corpus.file, ".//mw")
   mw.units.2 <- tolower(
      whitespace(
         gsub("\\s*<.*?>", " ",
              as.character(mw.units.1),
              perl=TRUE)
      )
   )
   mw.tags <- tolower(
      exact.matches.2(
         "(?<=<mw c5=\")[^\"]+",
         as.character(mw.units.1),
         gen.conc.output=FALSE)[[1]]
   )
   mws <- table(paste(mw.tags, mw.units.2, sep="\t"))

   # Now we are facing a somewhat complex problem: we must find all things tagged as words, but NOT again those words that are already covered by our inclusion of the multi-word units above. In file <A00.xml>, for example, the above found
   # <mw c5="AV0"><w c5="PRP" hw="at" pos="PREP">At </w><w c5="AV0" hw="least" pos="ADV">least </w></mw>
   # so we have that multi-word unit "At least", but do not want to count "At" and "least" again as single-word units, recall Figure 5.2 in the book for a similar example. Now you may think, no problem, I just proceed as follows to find only the word units that are direct children of s(entence) nodes:
   # w.units.1 <- xml_find_all(current.corpus.file, ".//s/w")
   # However, this will unfortunately not work, because <A00.xml> also contains this (in sentence 2):
   # <s n="2"><hi rend="bo"><w c5="NN1" hw="aids" pos="SUBST">AIDS </w><c c5="PUL">(</c><w c5="VVN-AJ0" hw="acquire" pos="VERB">Acquired </w><w c5="AJ0" hw="immune" pos="ADJ">Immune </w><w c5="NN1" hw="deficiency" pos="SUBST">Deficiency </w><w c5="NN1" hw="syndrome" pos="SUBST">Syndrome</w><c c5="PUR">)</c></hi><w c5="VBZ" hw="be" pos="VERB">is </w>
   # where, as you can see at the beginning, each word is at //s/hi/w ... Thus, what you really need is something like 'find w nodes only if they are not direct children of mw nodes. There are two ways to proceed:
   # Alternative 1 is kinda low-tech and deals with the problem by (i) deleting all mw-tags and what they tag in a flat version of the XML file and (ii) re-parsing the flat file into an XML object, which you can then search for any w nodes.
   # Alternative 2 is more in the spirit of XML and XPath but uses something that I didn't have enough space to discuss in more detail in the book, namely XPath axes and predicates: you describe in the path what I said above: w-nodes, but only when they are not children of mw nodes.

   # alternative 1 uses the following three lines
   corpus.file.flat <- as.character(current.corpus.file)                               # make the XML file a flat text file
   corpus.file.flat.wout.mwus <- gsub("<mw .*?</mw>", "", corpus.file.flat, perl=TRUE) # remove mw tags
   current.corpus.file <- read_xml(corpus.file.flat.wout.mwus)                         # re-import as XML

   w.units.1 <- xml_find_all(current.corpus.file, ".//w")
   # alternative 2 would be to replace the above line with the uncommented version of the next line:
   # w.units.1 <- xml_find_all(current.corpus.file, ".//w [not(parent::mw)]")
   w.units.2 <- tolower(
      whitespace(
         gsub("\\s*<.*?>", " ",
              as.character(w.units.1),
              perl=TRUE)
      )
   )
   w.tags <- tolower(
      exact.matches.2(
         "(?<=<w c5=\")[^\"]+",
         as.character(w.units.1),
         gen.conc.output=FALSE)[[1]]
   )
   ws <- table(paste(w.tags, w.units.2, sep="\t"))

   current.file.freqs <- c(mws, ws)
   save(current.file.freqs,  # save that frequency list into
        file=paste0(         # a file ...
           "../_outputfiles/05_10_freqoutput_xml2/", # whose path begins with the directory for these lists ...
           substr(basename(i), 1, 3), # whose name is the 3-character identifier of the BNC file name
           ".RData"))        # whose extension is .RData

} # end of loop

setwd("../_outputfiles/05_10_freqoutput_xml2") # move to the directory with all the file-specific frequency tables
# define an empty frequency table in which to collect all file-specific frequency tables
all.word.tags <- all.word.tag.freqs <- vector(length=25000000)
counter <- 1

for(i in dir()) {
   cat( # output a progress report to the screen
      basename(i), "\n"
      ) # the file that will be loaded on this iteration, followed by a line break
   load(i) # load the lemma frequency list from the i-th/current file

   all.word.tags[counter:(counter-1+length(current.file.freqs))] <- names(current.file.freqs)
   all.word.tag.freqs[counter:(counter-1+length(current.file.freqs))] <- current.file.freqs
   counter <- counter+length(current.file.freqs)
}

all.word.tag.freqs <- as.numeric(all.word.tag.freqs[nzchar(all.word.tags)])
all.word.tags <- all.word.tags[nzchar(all.word.tags)]

result <- tapply(      # the result of applying
   all.word.tag.freqs, # to all.word.tag.freqs
   all.word.tags,      # a grouping by the names of all.word.tag.freqs
   sum)                # the function sum
   class(result) <- "table" # make sure the data type is a table again
result <- sort(result, decreasing=TRUE) # sort the resulting table by frequency in descending order
setwd("../") # switch the working directory back one up

# storing the results
# as a csv file (which may not be opened completely in spreadsheet software given its large nrow of 1087587)
cat("TAG\tFORM\tFREQ",
    paste(names(result),
          result,
          sep="\t"),
    sep="\n",
    file="../10_output_xml2.csv")

temp <- strsplit(names(result), "\t")
result.df <- data.frame(
   TAGS=sapply(temp, "[", 1),
   FORMS=sapply(temp, "[", 2),
   FREQS=as.numeric(result))
save(result.df, file="../10_output_xml2.Rdata")



#########################################################################
# encoding problem: how can R decide which encoding to use automatically?
#########################################################################
cfs <- choose.path() # <AC0_part.xml> and <AC4_part.xml>

i <- 1 # <AC0.xml>, which is UTF-8
temp.conn<-file(cfs[i] , open="r", encoding="UTF-8")
 x1.u <- readLines(temp.conn) # works: <c c5=\"PUN\">…</c>
close(temp.conn)
temp.conn<-file(cfs[i] , open="r", encoding="ISO-8859-15")
 x1.i <- readLines(temp.conn) # reads it in but: <c c5=\"PUN\">â\u0080Š</c>
close(temp.conn)
# nchar and substr work


i <- 2 # <AC4.xml>, which is ISO-8859-15
temp.conn<-file(cfs[i] , open="r", encoding="UTF-8")
 x2.u <- readLines(temp.conn)
close(temp.conn)
temp.conn<-file(cfs[i] , open="r", encoding="ISO-8859-15")
 x2.i <- readLines(temp.conn)
close(temp.conn)
