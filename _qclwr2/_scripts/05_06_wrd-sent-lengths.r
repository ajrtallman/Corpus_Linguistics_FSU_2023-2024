rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for tk_choose_dir
source(file.choose()) # source the function exact.matches.2

# define the directory where the BNC files are located and get the file names
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# prepare collectors and results vectors
all.sent.lengths <- all.word.lengths <- vector(mode="list", length=length(corpus.files))

for(i in seq(corpus.files)) { # for the i-th file (i iterating over the numbers of all selected corpus files)
   cat( # output a progress report to the screen
      basename(corpus.files[i]), # the file that will be loaded on this iteration,
      "\t",                      # followed by a tab stop,
      i/length(corpus.files),    # followed by the percentage of files loaded
      "\n"  # followed by a line break
      ) # end of cat(...)

   current.corpus.file <- scan(corpus.files[i], what=character(), sep="\n", quiet=TRUE) # load the i-th/current corpus file

   # retrieve only the lines with sentences in them (i.e., discard the header, utterance tags, etc.)
   current.sentences <- grep("<s n=", # find the lines that have sentence number tags in them
                             current.corpus.file, # in the i-th/current corpus file
                             perl=TRUE,  # using Perl-compatible regular expressions
                             value=TRUE) # and return the (whole line) match, not its position index

   all.sent.lengths[[i]] <- as.numeric(table(     # put into slot i of the list a numeric vector of the frequency table of
      exact.matches.2("<w c5",                    # where you find the word tags
                      current.sentences,          # in current.sentences
                      gen.conc.output=FALSE)[[2]] # specifically, in which sentences you find the tags ([[2]] = the locations of matches)
   )) # end of table(...) and of as.numeric(...)

   current.wordtag.combos <- exact.matches.2( # retrieve all word-tag combinations
      "<w c5.*?</w>",                         # by finding the begining of a word tag till its end
      current.sentences,          # in current.sentences
      gen.conc.output=FALSE)[[1]] # but do not produce a concordance and retain only the exact matches

   all.word.lengths[[i]] <- nchar(  # determine the lengths in characters of
      gsub(" *<.*?>",               # what remains when you replace all tags (and preceding spaces, if any)
           "",                      # with nothing
           current.wordtag.combos,  # in current.wordtag.combos
           perl=TRUE)               # using Perl-compatible regular expressions
   ) # end of nchar(...)

} # end of loop

# compute average sentence and word lengths and inspect a summary of the vectors
summary(average.sent.lengths <- sapply(all.sent.lengths, median))
summary(average.word.lengths <- sapply(all.word.lengths, median))

# plot
plot(jitter(average.word.lengths),      # plot on the x-axis the average word length of each file (jittered)
     jitter(log(average.sent.lengths)), # plot on the y-axis the average sent length of each file (logged & jittered)
     xlab="", ylab="", axes=FALSE,      # don't plot axes or axis labels
     pch=16,                            # use filled circles as a plotting symbol/point character
     col=rgb(0, 0 , 0 , 25, maxColorValue=255)) # 0 red, 0 green, 0 blue, 25 alpha transparency on a scale from 0-255
   grid() # add a grid
   axis(side=1, col="blue", col.axis="blue") # add a blue bottom x-axis
      mtext(side=1,     # print text into the bottom margin
            "Median word lengths per sentence (in characters)", # namely this axis label
            col="blue", # in blue
            line=3)     # into the third line of the margin (that number needs to be determined by trial and error)
      abline(v=median(average.word.lengths), # add a vertical line at the median of the median word lengths
             col="blue", lty=2)              # in blue and dashed
   # insert median of the median lengths of words
   text(median(average.word.lengths), # plot text at x-coordinate of the median of the average word lengths
        log(100),                     # and the y-coordinate of log(100)
        paste0("Md=", median(average.word.lengths)), # a string that states the median of the average word lengths,
        pos=4, col="blue")            # plot the text slightly to the right of the coordinates and in blue

   axis(side=2, col="red", col.axis="red", # add a red left y-axis
        at=0:6,                  # with tickmarks at the positions from 0 to 6
        labels=round(exp(0:6),1) # with antilogged values at the tickmarks
        )
      mtext(side=2,     # print text into the left margin
            "Median sentence lengths (in words)", # namely this axis label
            col="red",  # in red
            line=3)     # into the third line of the margin (that number needs to be determined by trial and error)
      abline(h=median(log(average.sent.lengths)), # add a horizontal line at the median of the median sentence lengths
             col="red", lty=2)                    # in red and dashed
   # insert median of the sentence lengths in words
   text(5.5, # plot text at x-coordinate of 5.5
        median(log(average.sent.lengths)), # and the y-coordinate of the median of the sentence lengths in words
        paste0("Md=", median(average.sent.lengths)), # a string that states the median of the average sentence lengths
        pos=3, col="red")  # plot the text slightly above the coordinates and in red
   lines(lowess(log(average.sent.lengths) ~ # plot a smoother that relates the values on the y-axis
                   average.word.lengths),   # to those on the x-axis
         lwd=3) # with a heavy line
