rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.dir

# define the directory where the BNC corpus files are located and get choose some corpus files
corpus.files <- dir(rchoose.dir(), recursive=TRUE, full.names=TRUE)

# prepare collectors and results vectors
freq.sent <- freq.alph <- freq.ord <- freq.both <- 0 # define numeric vectors to store results

for(i in corpus.files) { # for the i-th file (i iterating over all selected corpus files)
   cat(basename(i), "\n") # output a 'progress report'
   current.corpus.file <- scan(i, what=character(), sep="\n", quiet=TRUE) # load the i-th/current corpus file
   current.sentences <- grep("<s n=", # find the lines that have sentence number tags in them
                             current.corpus.file, # in the i-th/current corpus file
                             perl=TRUE,  # using Perl-compatible regular expressions
                             value=TRUE) # and return the (whole line) match, not its position index

   if(any(grepl("</w>", current.sentences))) { # check whether there is at least one closing word tag in the sentences
      # if you find closing word tags, you loaded an XML version
      search.expression.alph <- "<w c5=\"AJ0\"[^>]*>alphabetical\\b"
      search.expression.ord  <- "<w c5=\"NN1\"[^>]*>order\\b"
   } else {
      # if you don't find closing word tags, you loaded an SGML version
      search.expression.alph <- "<w AJ0>alphabetical\\b"
      search.expression.ord  <- "<w NN1>order\\b"
   } # end of the conditional expression

   # compute the number of searched sentences
   freq.sent <-                 # make the new value of the number of sentences
      freq.sent +               # the old value of the number of sentences plus ...
      length(current.sentences) # ... the number of sentences of this i-th/current file

   # search for alphabetical
   search.alph <- grep( # look for sentences containing
      search.expression.alph, # alphabetical as a base form adjective followed by a word boundary
      current.sentences,      # in current.sentences
      ignore.case=TRUE,       # case-insensitively
      perl=TRUE)              # using Perl-compatible regular expressions
   freq.alph <- freq.alph + length(search.alph)

   # search for order and add the number of sentences to freq.ord
   search.ord <- grep( # look for sentences containing
      search.expression.ord, # order as a sing noun followed by a word boundary
      current.sentences,     # in current.sentences
      ignore.case=TRUE,      # case-insensitively
      perl=TRUE)             # using Perl-compatible regular expressions
   freq.ord <- freq.ord + length(search.ord)

   freq.both <- freq.both + length(intersect(search.alph, search.ord)) # the intersection of the two sets of sentence numbers
} # end of loop

# the results in a 2x2 matrix like this
#             ORDER
# ALPHABETICAL  +     -      Sum
#          +    a     b      a+b
#          -    c     c      c+d
#        Sum  a+c   a+c  a+b+c+d
cell.a <- freq.both                             # +alph, +ord
cell.b <- freq.alph - freq.both                 # +alph, -ord
cell.c <- freq.ord - freq.both                  # -alph, +ord
cell.d <- freq.sent-sum(cell.a, cell.b, cell.c) # -alph, -ord
coocc.alph.order <- matrix( # generate a matrix of the values from this vector
   c(cell.a,   # +alph, +ord
     cell.b,   # +alph, -ord
     cell.c,   # -alph, +ord
     cell.d),  # -alph, -ord
   ncol=2,        # make that matrix have two columns
   byrow=TRUE,    # recognize the values are provided by row
   dimnames=list( # use these dimension names for the matrix
      ALPHABETICAL=c("+", "-"), # for the rows
      ORDER=c("+", "-")
      )        # for the columns
   ) # end of matrix(...)
addmargins(coocc.alph.order)

# compute Mutual Information: log_2 of observed frequency / expected frequency
log2(                          # compute the binary log of
   coocc.alph.order["+","+"] / # the observed co-occurrence frequency divided by
                               # the expected co-occurrence frequency as obtained from
      chisq.test(coocc.alph.order, # applying a chi-squared test to the above matrix
                 correct=FALSE)$expected["+","+"] # without correction, retrieving the expected frequency for +, +
) # 6.486706

# Delta Ps:
# alphabetical predicting order
(cell.a/freq.alph) - (cell.c/(cell.c+cell.d)) # 0.4219247

# order predicting alphabetical
(cell.a/freq.ord) - (cell.b/(cell.b+cell.d)) # 0.003327867
