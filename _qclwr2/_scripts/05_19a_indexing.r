rm(list=ls(all=TRUE)) # clear memory
source(file.choose()) # source exact.matches.2

# load the data
book.file <- readLines(con <- file( # read in lines from a file connection to
   file.choose(),                   # <_qclwr2/_inputfiles/corp_indexing-1.txt>
   encoding="UTF-8"),               # which is in UTF-8 encoding
   warn=FALSE); close(con)          # suppress warnings and close the connection

book.file.one.string <- paste(book.file,    # paste the vector book.file into one character string
                              collapse=" ") # with spaces between the original vector elements

book.file.pages <- unlist(strsplit( # unlist the list you get from splitting
   book.file.one.string, # the 1-element character vector book.file.one.string
   " *09:39:08:01:09 *", # at a code that is occurs once at the bottom of each page
   perl=TRUE             # using Perl-compatible regular expressions
   ))[-1]                 # and omit first empty string

# extract lines that contain R code
lines.with.code <- exact.matches.2("[>+].*?¶",
                                     book.file.one.string)[[1]]

# extract strings that could be functions
function.candidates <- sort(unique(      # sort the unique types of strings that match
   exact.matches.2("[_a-z\\.]+(?=\\()",  # 1+ letters, periods, or underscores before opening parentheses
                   lines.with.code,      # in the character vector lines.with.code
                   case.sens=FALSE)[[1]] # case-insensitively and return just the exact matches
)) # end of unique(...), end of sort(...)
function.candidates # looks pretty good!



# first way to create an index for these function names the elementary way
index1 <- list()
for(current.function in function.candidates) {
   index1[[current.function]] <- # put into the index entry for the function candidate
      grep(               # the result of searching (for the position indices, note: value=FALSE) for
         paste(               # the pasting together of
            "\\b",            # a word boundary, followed by
            current.function, # the current function candidate, followed by
            "\\b",            # a word boundary
            sep=""),          # with no character between those elements
         book.file.pages, # in the character vector book.file.pages
         perl=TRUE        # using Perl-compatible regular expressions
      ) # end of grep(...)
} # end of loop

# first way to output the index (just to the screen) the elementary way
for(i in seq(index1)) {
   cat(paste(                # the pasting together of
      names(index1)[[i]],    # the current function name, followed by
      "\t",                  # a tab stop, followed by
      paste(index1[[i]],     # the page number(s)
            collapse=", "),  # with commas and spaces between page numbers
      sep=""),               # but no additional spaces
      "\n",                  # add a line break after an index entry
      sep="")                # but nothing else
} # end of loop



# second and nicer way to create an index for these function names
index2 <- list()
index2[function.candidates] <- lapply( # apply to
   paste(                              # the pasting together of
      "\\b",                           # a word boundary, followed by
         function.candidates,          # the function candidates, followed by
         "\\b",                        # a word boundary
         sep=""),                      # with no character between those elements
   grep,              # the function grep, i.e. searching (for position indices, note the default of value=FALSE)
   x=book.file.pages, # searching in the character vector book.file.pages
   perl=TRUE)         # using Perl-compatible regular expressions



# second and nicer way to output the index (just to the screen)
index.2.cat <- sapply(index2,        # apply to each element in index2
                      paste,         # a pasting together
                      collapse=", ") # into one string with ", " between the elements
cat(paste(             # print to the screen the pasting together of
   names(index.2.cat), # the names of index.2.cat, i.e., the function names with
   index.2.cat,        # the comma-separated page numbers
   sep="\t"),          # with a tab between function name and page numbers
   sep="\n")           # and a separate line for each index entry




# excursus: here are some first passes of how one could capture function names used in sapply and tapply calls:
# note: in this part of the book, there is no tapply example so I tried it on "tapply(matches,∙matches,∙sum)¶"
sapply.matches <- gsub(
   "sapply\\(.*?,∙([^,)]+).*",
   "\\1",
   exact.matches("sapply\\(.*?[ ,][^,]+",
                 lines.with.code)[[1]],
   perl=TRUE); sort(unique(sapply.matches))

tapply.matches <- gsub(
   "tapply\\(.*,∙([^,)]+).*",
   "\\1",
   exact.matches("tapply\\([^∙,]+,∙[^∙,]+,∙[^∙,]+\\)",
                 lines.with.code)[[1]],
   perl=TRUE); sort(unique(tapply.matches))
