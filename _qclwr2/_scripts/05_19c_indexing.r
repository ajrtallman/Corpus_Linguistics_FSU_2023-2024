rm(list=ls(all=TRUE)) # clear memory

# load the data
paper.file <- readLines(           # read in lines
   con <-file(file.choose(),       # from a connection to the user-defined file <_qclwr2/_inputfiles/corp_indexing-2.txt>
              encoding="UCS-2LE")) # that is in this encoding
close(con)                         # close the connection again, would also happen automatically here
paper.file[402] # ok, no problem with "ΔP"

paper.file.one.string <- paste(paper.file,   # paste the vector paper.file into one character string
                               collapse=" ") # with spaces between the original vector elements

paper.pages <- unlist(strsplit(             # unlist the list you get from splitting
   paper.file.one.string,                   # the 1-element character vector paper.file.one.string
   " *John Benjamins Publishing Company *", # at a string that occurs once at the bottom of each page
   perl=TRUE                                # using Perl-compatible regular expressions
   ))[-1]                                   # and omit the first empty string
paper.pages <- tolower(paper.pages) # then change the vector to lower case
paper.pages <- gsub(" *Frequencies, probabilities, and association measures in usage-/exemplar-based linguistics *", # replace header/footer
                    " ",         # by a space
                    paper.pages, # in the character vector paper.pages
                    perl=TRUE)   # using Perl-compatible regular expressions

# definining a few index terms
index.terms <- sort(c("collostruction", "(association measure|\\bam\\b)",
                      "(fisher-yates|fye)", "zipf", "frequenc(y|ies)", "as-predicative",
                      "ditransitive", "type-token", "entrop(y|ies)", "dispersion"))

# creating an index for these terms
index <- list()
index[index.terms] <- lapply( # apply to each of
   index.terms,               # the index terms
   grep,                      # the function grep, i.e. searching (for position indices, note the default of value=FALSE)
   x=paper.pages,             # searching in the character vector paper.pages
   perl=TRUE)                 # using Perl-compatible regular expressions



# merge consecutive page numbers into page ranges
# let's go through the merging of page ranges step by step before
# we do it in a loop to the whole index ...:
(qwe <- index[[ifelse(.Platform$OS.type=="windows", 1, 2)]]) # because Windows and Linux differ in their alphabetical sorting ...
(ranges <- ifelse(diff(qwe)==1, # if the difference between neighboring numbers is 1
                  "-",       # store a "-",
                  ","))      # otherwise a ","
(all.in.one <- paste(qwe,                   # paste the index numbers together with
                     ranges,                # the above elements from ranges
                     sep="", collapse=" ")) # no intervening materials, all into one string
(all.in.one <- substr(all.in.one, 1, nchar(all.in.one)-1)) # omit the last character of all.in.one
gsub("(\\d+)- (\\d+- )*(\\d+)", # replace (sequences of numbers) followed be "-" and some last number
     "\\1-\\3",                 # by the first and the last number, with a "-" inbetween
     all.in.one,                # in the character vector all.in.one
     perl=TRUE)                 # using Perl-compatible regular expressions



# ok, now to the whole index:
for(i in seq(index)) { # for the i-th element of the list called index
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
cat(paste(       # print to the screen the pasting together of
   names(index), # the names of index, i.e., the index terms with
   index,        # the comma-separated page numbers
   sep="\t"),    # with a tab between the index term name and the page numbers
   sep="\n")     # and a separate line for each index entry

# once you're done with this script and have understood it, check exercise 5 for chapter 5