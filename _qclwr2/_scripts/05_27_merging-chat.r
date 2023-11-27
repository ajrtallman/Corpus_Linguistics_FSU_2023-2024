rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.files()

# define the corpus files (in <_qclwr2/_inputfiles/corp_27_coralrom/>)
chat.files <- rchoose.files()

# define data structures for the loops to follow
chat.files.list <- list() # create a list that will collect all utterances and their annotation
utt.counter <- 0          # set to 0 a counter that will increase with each line that contains an utterance

for(i in seq(chat.files)) { # access each corpus file
   cat(basename(chat.files[i]), "\t:\t", i/length(chat.files), "\n") # output progress report

   current.chat.file.01 <- readLines(con <- file(chat.files[i], encoding="ISO-8859-15"),
                                     warn=FALSE); close(con)

   current.chat.file.02 <- paste(current.chat.file.01, # paste each corpus file together into one string
                                 collapse="_dReck_")   # with line breaks now replaced by something that shows up in no file

   current.chat.file.03 <- gsub("___\\s+", " ",       # replace formerly line-initial whitespace by just a space
                                current.chat.file.02, # in current.chat.file.02
                                perl=TRUE)            # using Perl-compatible regular expressions

   current.chat.file.04 <- unlist(strsplit(current.chat.file.03, # split current.chat.file.03 up where
                                           "_dReck_"))           # line breaks used to be

   current.chat.file.05 <- grep("^[\\*%]...:",        # look for lines beginning with * or %, followed by 3 characters
                                current.chat.file.04, # in current.chat.file.04
                                perl=TRUE,            # using Perl-compatible regular expression
                                value=TRUE)           # return the lines, not just their positions

   # extract the corpus file date from the header
   chat.file.date <- gsub("^@date: ", "", # delete the beginning of the line you find when you
                          grep("^@date: ", current.chat.file.01, # look for the date line
                               perl=TRUE, value=TRUE,  # using Perl-compatible regular expressions and keeping the line
                               ignore.case=TRUE),      # case-insensitively
                          perl=TRUE, ignore.case=TRUE) # using Perl-compatible regular expressions and case-insensitively

   # convert to a list
   for(j in seq(current.chat.file.05)) { # access each line of this corpus file

      line.type <- substr(current.chat.file.05[j], 1, 1)       # extract the first character of the j-th/current line (* or %))
      line.identifier <- substr(current.chat.file.05[j], 2, 4) # extract the name of the speaker or the type of annotation
      line.content <- substr(current.chat.file.05[j], 7,       # extract the content of line, starting at character 7 and
                             nchar(current.chat.file.05[j]))   # ending at its end

      if(line.type=="*") { # if this line contains a new utterance (because it begins with a speaker name annotation)
         utt.counter <- utt.counter+1 # increment the utterance counter for this new utterance
         tier.counter <- 1            # reset back tier.counter to 1 for whatever annotation the new utterance might have in the lines to come

         chat.files.list[["file"]][utt.counter] <-basename(chat.files[i]) # record the file name for this new utterance
         chat.files.list[["names"]][utt.counter] <- line.identifier       # record the speaker name for this new utterance
         chat.files.list[["date"]][utt.counter] <- chat.file.date         # record the age for this new utterance
         chat.files.list[["utterance"]][utt.counter] <- line.content      # record the utterance for this new utterance

      } else { # if this line contains annotation ...
         if(length(chat.files.list[[line.identifier]])<utt.counter) { # if there is not yet any annotation with this name for this utterance
            chat.files.list[[line.identifier]][utt.counter] <- # add to the list component that has this annotation name and belongs to the currently-processed utterance
               line.content                                    # the content of this line, i.e. the current utterance's annotation
         } else {                          # otherwise (i.e. if there is already some annotation of this type recorded for this utterance)
            tier.counter <- tier.counter+1 # increment the repeated-tier counter by one to create a separate name for this annotation
            chat.files.list[[paste(        # add to the list component that gets the name of
               line.identifier, tier.counter, sep="_")]][utt.counter] <- # this annotation tier plus a disambiguating number
               line.content # the content of this line, i.e. the current utterance's annotation
         } # end of conditional expression (checking whether there is already annotation with this name for this utterance)
      } # end of conditional expression (checking whether we are processing a new utterance or an already-processed utterance's annotation)
   } # end of inner loop j (going through lines of a file)
} # end of outer loop i (accessing each corpus file)

# resort the elements of the list alphabetically (but leave the file name, speaker name, the date, and the utterance at the left as is)
chat.files.list <- chat.files.list[c(1:4, order(names(chat.files.list)[-c(1:4)])+4)]

# convert the corpus into a data frame
chat.files.dataframe <- as.data.frame( # make into a data frame
   sapply(             # the result of applying
      chat.files.list, # to the list containing all the corpus data
      "[",             # the function extract/subset, namely extracting
      1:               # from element 1 to
         max(sapply(chat.files.list, length)))) # the maximal length of a list element (-> nrow of the data frame)

# save the data frame into a tab-delimited CSV file
write.table(chat.files.dataframe, file="../_outputfiles/05_27_merging-chat.csv",
            sep="\t", eol="\n", quote=FALSE, row.names=TRUE, col.names=NA)
