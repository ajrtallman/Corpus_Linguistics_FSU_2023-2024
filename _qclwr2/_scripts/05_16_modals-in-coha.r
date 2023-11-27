rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for rchoose.files

ranger <- function (positions, before.and.after=5, desired.min=1, desired.max=max(positions), padded=TRUE, with.center=TRUE) {
   if(!padded) {
      output.list <- lapply(positions, function(x) {
         seq(from=max(desired.min, x-before.and.after),
             to=min(desired.max, x+before.and.after)) } )
   } else {
      output.list <- lapply(positions, function(x) {
         seq(from=x-before.and.after,
             to=x+before.and.after) } )
      output.list <- lapply(output.list,
                            function (x) replace(x,
                                                 (x<desired.min|x>desired.max),
                                                 NA))
   }
   names(output.list) <- positions
   if(!with.center) {
      for(i in seq(output.list)) {
         output.list[[i]][output.list[[i]]==positions[i]] <- NA
      }
   }
   return(output.list)
} # end of function definition

# this is what ranger does:
set.seed(3)
qwe <- sample(letters, 100, replace=TRUE) # a 'corpus' of length 100 and some ...
which(qwe=="e")                           # ... 'concordance output': we're looking for "e"
asd <- ranger(              # set asd to the result of ranger, namely when we get 5 left and 5 right collocates
   which(qwe=="e"),         # of the positions where we find "e"
   desired.max=length(qwe)) # set to the length of the corpus vector to which subsetting will be applied
asd
# when 5 to the left and 5 to the right is possible (given the length of the corpus vector),
#    then you get those collocate slots, as in elements 2:5 of asd
# when they are not, you get as many as possible, as in elements 1 and 6 of asd



coha.directories <- dir(rchoose.dir(), full.names=TRUE)[1:9] # pick only the directories for 1810 to 1890
coha.files <- unlist(                 # generate a vector with all files by unlisting
   sapply(                            # the list resulting from sapply-ing
      coha.directories,               # to all coha directories
      dir,                            # the function dir
      full.names=TRUE, recursive=TRUE # with these arguments (as usual)
   ) # end of sapply(...)
) # end of unlist(...)

search.terms <- c("will", "shall")
window.left.and.right <- 3

# create a matrix to collect all collocates
results <- matrix(ncol=2*window.left.and.right+1, # generate a matrix with, here, 2*3+1=7 columns
                  dimnames=list(                  # and
                     NULL,                        # no row names
                     -window.left.and.right:window.left.and.right) # column names from -3 to +3
                  ) # end of matrix(...)



for(i in seq(coha.files)) { # access each corpus file
   cat(basename(coha.files[i]), # print the basename of the i/th/current corpus file
       "\t:\t",                 # followed by a tab, a colon, and another tab, followed by
       i/length(coha.files),    # the percentage of files dealt with, followed by
       "\n")                    # a line break

   # load the i-th/current corpus file
   current.file <- tolower(readLines(     # put into current.file the result of changing to lower case lines
      con <- file(coha.files[i],          # read from a connection to the i-th COHA file
                  encoding="ISO-8859-1"), # with the Western European encoding
      warn=FALSE)[-1]); close(con)        # suppress warnings, discard the first (header) line, and close the connection again
   current.file <- strsplit(current.file, "\t") # split that file up at tab stops

   current.lemmas <- sapply(current.file, "[", 2)  # from the elements of the list current.file, extract all 2nd ones
   current.lemmas[!nzchar(current.lemmas)] <- "NA" # to handle cases without a lemma/form
   current.tags <- sapply(current.file, "[", 3)    # from the elements of the list current.file, extract all 3rd ones

   verbmodal.tag.positions <- grep("vm",         # find the position of modal verb tags
                                   current.tags, # in current.tags
                                   perl=TRUE)    # using Perl-compatible regular expressions

   for(j in search.terms) { # for each of the search terms (here just the two, will and shall)
      search.term.positions <- which(current.lemmas==j) # find the positions of the search term in the lemmas
      search.term.as.modals <- intersect(search.term.positions,   # retain only those occurrences of the search term
                                         verbmodal.tag.positions) # that are also tagged as modal verbs

      collocate.slots <- ranger(search.term.as.modals,                  # create the collocate positions for the relevant matches
                                before.and.after=window.left.and.right, # going 3 lemmas to the left and 3 to the right
                                desired.min=1,                          # where the corpus file starts at lemma 1
                                desired.max=length(current.lemmas))     # and ends at its last lemma

      collocates <- matrix(                          # generate a matrix
         current.lemmas[unlist(collocate.slots)],    # of the lemmas that are in the collocate slots just found
         byrow=TRUE, ncol=2*window.left.and.right+1, # organize them by row into as many columns as needed (7)
         dimnames=list(                              # and assign dimension names
            substr(                                  # namely row names that are the first 4 characters
               sub("^.*?_", "",                      # after deleting the genre part
                   rep(basename(coha.files[i]), length(collocate.slots)), perl=TRUE), # from the corpus file
               1, 4),
            -window.left.and.right:window.left.and.right) # and column names that numerically indicate the collocate slot
      ) # end of matrix(...)
      results <- rbind(results, collocates)                # and add those collocates to the previously collected ones
   } # end of inner loop (accessing search terms)
} # end of outer loop (accessing corpus files)

results <- results[-1,] # delete the first row of NAs that was used to create the matrix before the loop
results <- data.frame(                                   # save the results in a data frame with columns
   YEAR=as.numeric(rownames(results)),                   # for the year
   DECADE=sub(".$", "_", as.numeric(rownames(results))), # for the decade
   L3=results[,1], L2=results[,2], L1=results[,3],       # for all the left collocate slots
   NODE=results[,4],                                     # for the node word
   R1=results[,5], R2=results[,6], R3=results[,7]        # for all the right collocate slots
)
save(results, file="../_outputfiles/05_16_modals-in-coha-dataframe.RData") # and save the data frame into a file

results.split <- split(results,           # split the data frame up by
                       list(              # the combination of
                          results$DECADE, # the decade
                          results$NODE    # and the node word
                          ))
all.top.15.collocates <- lapply(results.split,      # apply to each element of that list
                                summary, maxsum=15) # the function summary to get the 15 most frequent collocates

# inspect some results
all.top.15.collocates[[1]]  # shall 1810-1819
all.top.15.collocates[[2]]  # shall 1820-1829
all.top.15.collocates[[10]] # will 1810-1819
# etc.



###################################################################################################



# And here's a version of ranger that allows you to define different numbers of left and right collocates
rm(list=ls(all=TRUE)) # clear memory
ranger <- function (positions, before=5, after=5, desired.min=1, desired.max=max(positions), padded=TRUE, with.center=TRUE) {
   if(!padded) {
      output.list <- lapply(positions, function(x) {
         seq(from=max(desired.min, x-before),
             to=min(desired.max, x+after)) } )
   } else {
      output.list <- lapply(positions, function(x) {
         seq(from=x-before,
             to=x+after) } )
      output.list <- lapply(output.list,
                            function (x) replace(x,
                                                 (x<desired.min|x>desired.max),
                                                 NA))
   }
   names(output.list) <- positions
   if(!with.center) {
      for(i in seq(output.list)) {
         output.list[[i]][output.list[[i]]==positions[i]] <- NA
      }
   }
   return(output.list)
} # end of function definition

# this is what ranger does:
set.seed(3)
qwe <- sample(letters, 100, replace=TRUE) # a 'corpus' of length 100 and some ...
which(qwe=="e")                           # ... 'concordance output': we're looking for "e"
asd <- ranger(before=3, after=6,          # set asd to the result of ranger, with 3 left and 6 right collocates
   which(qwe=="e"),         # of the positions where we find "e"
   desired.max=length(qwe)) # set to the length of the corpus vector to which subsetting will be applied
asd
# when 3 to the left and 6 to the right is possible (given the length of the corpus vector),
#    then you get those collocate slots, as in elements 2:5 of asd
# when they are not, you get as many as possible, as in elements 1 and 6 of asd
