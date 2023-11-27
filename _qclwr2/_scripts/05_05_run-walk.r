rm(list=ls(all=TRUE)) # clear memory
library(rChoiceDialogs) # load the library for tk_choose_dir
source(file.choose()) # source the function exact.matches.2

# define the directory where the BNC corpus files are located
corpus.files <- dir(rchoose.dir(), full.names=TRUE, recursive=TRUE)

# define a logical vector that prompts the user to declare whether he wants to use xml files or not
xml.version <- switch( # create a vector xml.version
   menu(                               # by letting the user choose from a menu
      c("XML", "SGML"),                # the option "XML" or the option "SGML"
      graphics=TRUE,                   # in a graphic/window pop-up
      title="What's the annotation?"), # with this title
   TRUE,               # make xml.version be TRUE if the user chooses "XML"
   FALSE               # make xml.version be FALSE if the user chooses "SGML"
) # end of switch(...)

# define search expressions depending on which kind of corpus annotation will be processed
if(xml.version) { # if xml.version is TRUE (i.e., you're looking at the XML files)
   search.expression.run  <- "<w c5=\\\"[vn][^>]+>runs?(?= ?<)"
   search.expression.walk <- "<w c5=\\\"[vn][^>]+>walks?(?= ?<)"
   search.expression.tags.coarse <- "(?<=pos=\\\")[^\"]+" # find what follows pos="
   search.expression.tags.fine   <- "(?<=c5=\\\")[^\"]+"  # find what follows c5="
} else {          # if xml.version is FALSE (i.e., you're looking at the SGML files)
   search.expression.run  <- "<w [vn][^>]+>runs?(?!([-a-z]| [a-z]))"
   search.expression.walk <- "<w [vn][^>]+>walks?(?!([-a-z]| [a-z]))"
   search.expression.tags.coarse  <- "(?<=<w )."  # find the one character that follows <w
   search.expression.tags.fine <- "(?<=<w )[^>]+" # find all characters that follows <w  till >
} # end of the conditional expression
search.expression.forms <- "^.*>" # find everything till and including the last > (same as both, hence outside the conditional)

# prepare collectors and results vectors
all.runs <- all.walks <- character()

for(i in corpus.files) { # for the i-th file (i iterating over the numbers of all selected corpus files)
   cat(basename(i), "\n") # print the name of the file loaded on this iteration and a line break
   current.corpus.file <- scan(i, what=character(), sep="\n", quiet=TRUE) # load the corpus file i

   # retrieve only the lines with sentences in them (i.e., discard the header, utterance tags, etc.)
   current.sentences <- grep("<s n=", # find the lines that have sentence number tags in them
                             current.corpus.file, # in the i-th/current corpus file
                             perl=TRUE,  # using Perl-compatible regular expressions
                             value=TRUE) # and return the (whole line) match, not its position index
   current.sentences <- tolower(current.sentences) # switch the corpus file to lower case

   # looking for run/runs and walk/walks (with whatever tag)
   all.runs <- c(      # make the new content of all.runs
      all.runs,        # the old content of all.runs plus
      exact.matches.2( # the result of the search for
         search.expression.run,  # run/runs with the tags
         current.sentences,      # in current.sentences
         gen.conc.output=FALSE)[[1]] # but do not produce a concordance and retain only the exact matches
   )
   all.walks <- c(     # make the new content of all.walks
      all.walks,       # the old content of all.walks plus
      exact.matches.2( # the result of the search for
         search.expression.walk, # walk/walks with the tags
         current.sentences,      # in current.sentences
         gen.conc.output=FALSE)[[1]] # but do not produce a concordance and retain only the exact matches
   )
} # end of loop

tags.coarse <- c( # create a vector tags.coarse that contains
   exact.matches.2(search.expression.tags.coarse, # the exact matches of the coarse tags
                   all.runs,                      # in all.runs
                   gen.conc.output=FALSE)[[1]],   # and suppress the concordance output
   exact.matches.2(search.expression.tags.coarse, # the exact matches of the coarse tags
                   all.walks,                     # in all.walks
                   gen.conc.output=FALSE)[[1]]    # and suppress the concordance output
)
tags.fine <- c( # create a vector tags.fine that contains
   exact.matches.2(search.expression.tags.fine, # the exact matches of the fine-grained tags
                   all.runs,                    # in all.runs
                   gen.conc.output=FALSE)[[1]], # all the C5 tags of the matches for runs? and
   exact.matches.2(search.expression.tags.fine, # the exact matches of the fine-grained tags
                   all.walks,                   # in all.walks
                   gen.conc.output=FALSE)[[1]]  # all the C5 tags of the matches for walks?
)
forms <- c( # create a vector tags.fine that contains the combination of these two:
   gsub(search.expression.forms, # replace everything but the word form used
        "",                      # by nothing
        all.runs,                # in all.runs
        perl=TRUE),              # using Perl-compatible regular expressions
   gsub(search.expression.forms, # replace everything but the word form used
        "",                      # by nothing
        all.walks,               # in all.walks
        perl=TRUE)               # using Perl-compatible regular expressions
)

# store all results in one data frame
result <- data.frame(      # create a dataframe result with
   TAGSCOARSE=tags.coarse, # a column for all coarse tags
   TAGSFINE=tags.fine,     # a column for all fine/C5 tags
   FORMS=forms,            # a column for all forms
   LEMMAS=ifelse(substr(forms, 1, 1)=="r", # a column for all lemmas, depending on the first letter of the form:
                 "run",  # if that's an r, make the lemma "run"
                 "walk") # otherwise, make the lemma "walk"
)
summary(result) # inspect that dataframe result

# split the results up be lemma
results <- split(result, result$LEMMAS) # split up result into a list depending on the lemma
lapply(results, summary) # inspect that list results: note how R keeps unused factor levels (e.g. in LEMMAS)

results <- lapply(results,    # make a new results by applying to each data frame in results
                  droplevels) # the function droplevels, which drops unused factor levels
lapply(results, summary) # inspect each list element's summary



# do some very simple statistics (here just on the results for runs?)
(results.run.coarse <- with(results$run,                # with the runs results
                            table(TAGSCOARSE, FORMS)))  # cross-tabulate the coarse tags and the forms
chisq.test(results.run.coarse, correct=FALSE)           # run a chi-squared test on that 2x2 table
chisq.test(results.run.coarse, correct=FALSE)$expected  # check the expected frequencies
chisq.test(results.run.coarse, correct=FALSE)$residuals # and the residuals
assocplot(t(results.run.coarse))                        # and an association plot (transposed input table!)
mosaicplot(t(results.run.coarse))                       # and a mosaic plot (transposed input table!)

# compute an effect size for that table according to the formula in Section 5.2.2
numerator <- chisq.test(results.run.coarse, correct=FALSE)$statistic
denominator <- sum(results.run.coarse)
sqrt(numerator/denominator)
# highly significant, but really very small effect size

# a quick glance at the finer resolution of the C5 tags
(results.run.fine <- with(results$run,             # with the runs results, drop all unused levels and
                          table(TAGSFINE, FORMS))) # cross-tabulate the fine/C5 tags and the forms



# do some very simple statistics (here just on the results for walks?)
results.walk.coarse <- with(results$walk,                # with the walks results
                           table(TAGSCOARSE, FORMS))     # cross-tabulate the coarse tags and the forms
chisq.test(results.walk.coarse, correct=FALSE)           # run a chi-squared test on that table
chisq.test(results.walk.coarse, correct=FALSE)$expected  # check the expected frequencies
chisq.test(results.walk.coarse, correct=FALSE)$residuals # and the residuals
assocplot(t(results.walk.coarse))                        # and an association plot (transposed input table!)
mosaicplot(t(results.walk.coarse))                       # and a mosaic plot (transposed input table!)

# compute an effect size for that table according to the formula in Section 5.2.2
numerator <- chisq.test(results.walk.coarse, correct=FALSE)$statistic
denominator <- sum(results.walk.coarse)
sqrt(numerator/denominator)
# highly significant, but and small effect size (but much larger than the one for run)

# a quick glance at the finer resolution of the C5 tags
(results.walk.fine <- with(results$walk,            # with the walks results, drop all unused levels and
                           table(TAGSFINE, FORMS))) # cross-tabulate the fine/C5 tags and the forms



# Note: the results differ a bit between the BNC XML and the BNC SGML version, which may in part be due to the fact that the latter has 5 more files than the former.



# And this is how you would do the searches for both words in one regular expression:
search.expression.run.and.walk <- "(?x) # set free-spacing
<w\\s      # the beginning of a word tag
c5=\"[vn]  # with a c5-tag beginning with v' or 'n'
[^>]+>     # followed by the rest of the tag
(run|walk) # followed by 'run' or 'walk'
s?         # followed by an optional s
(?=\\s?<)" # but only if you can look to the right and see an optional space and a '<'
exact.matches.2(search.expression.run.and.walk, current.sentences, gen.conc.output=FALSE)[[1]]
