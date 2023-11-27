rm(list=ls(all=TRUE)) # clear memory

# load the data
book.file <- readLines(con <- file( # read in lines from a file connection to
   file.choose(),                   # <_qclwr2/_inputfiles/corp_indexing-1.txt>
   encoding="UTF-8"),               # which is in UTF-9 encoding
   warn=FALSE); close(con)          # suppress warnings and close the connection

book.file.one.string <- paste(book.file,    # paste the vector book.file into one character string
                              collapse=" ") # with spaces between the original vector elements

book.file.pages <- unlist(strsplit( # unlist the list you get from splitting
   book.file.one.string, # the 1-element character vector book.file.one.string
   " *09:39:08:01:09 *", # at a code that is occurs once at the bottom of each page
   perl=TRUE             # using Perl-compatible regular expressions
   ))[-1]                # and omit first empty string



# get a list of all word types one might want to index, e.g.,
# everything that consists (only) of minuses, letters, numbers, underscores, and periods
words.tokens <- unlist(strsplit(
   book.file.one.string,
   "[^-a-zA-Z0-9_\\.]+",
   perl=TRUE))
word.types.1 <-unique(words.tokens) # create list of types
word.types.2 <-gsub("(^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$)", # find types with non-letters/-numbers at beginnings and ends
                    "",                                # replace them by nothing
                    word.types.1,                      # in word.types.1
                    perl=TRUE)                         # using Perl-compatible regular expressions
word.types.3 <- gsub("^\\d+(\\.\\d+)+$", # find strings that just consist of integers or decimal numbers
                     "NUM",              # replace those by "NUM"
                     word.types.2,       # in word.types.2
                     perl=TRUE)          # using Perl-compatible regular expressions
word.types.4 <- sort(unique(word.types.3[nzchar(word.types.3)])) # delete empty strings



# creating an index for all words (note: this index is case-sensitive - how would you change that?)
index <- list()
index[word.types.4] <- lapply( # apply to
   paste(                      # the pasting together of
      "\\b",                   # a word boundary, followed by
      word.types.4,            # all word types in the (part of the) book
      "\\b",                   # a word boundary
      sep=""),                 # with no character between those elements
   grep,                       # the function grep, i.e. searching (for position indices, note the default of value=FALSE)
   x=book.file.pages,          # searching in the character vector book.file.pages
   perl=TRUE)                  # using Perl-compatible regular expressions

# the nice way to output the index (just to the screen)
index.2.cat <- sapply(index,         # apply to each element in the index
                      paste,         # a pasting together
                      collapse=", ") # into one string with ", " between the elements
cat(paste(             # print to the screen the pasting together of
   names(index.2.cat), # the names of index.2.cat, i.e., the function names with
   index.2.cat,        # the comma-separated page numbers
   sep="\t"),          # with a tab between function name and page numbers
   sep="\n")           # and a separate line for each index entry



# PS: this is how one can generate quick user-defined queries
(regexes <- index[grep("(grep|g?sub|g?regexpr|strsplit)", # find certain strings in
                       names(index),                      # the names of the list index
                       perl=TRUE)]                        # using Perl-compatible regular expressions
)

sort(                         # sort
   unique(                    # the types
      unlist(                 # of the unlisted (i.e. vectorized)
         regexes[select.list( # elements of regexes that the user selects
            names(regexes),   # from all names of regexes looked for above
            multiple=TRUE,    # (one can choose more than just one)
            graphics=TRUE)]   # (in a graphics window)
      )                       # end of unlist
   )                          # end of unique
)                             # end of sort
