exact.matches.2 <- exact.matches.new <- function (search.expression, corpus.vector, pcre=TRUE, case.sens=TRUE, characters.around=0, lines.around=0, lazy=FALSE, clean.up.spaces=TRUE, gen.conc.output=TRUE, vectorize=TRUE) {
   # rm(list=ls(all=TRUE))
   # corpus.vector <- txt <- c("This is a first example sentence.", "qwerty", "And this is a second example sentence.")
   # beg <- "s"; end <- "s"; search.expression <- c(beg, end)
   # pcre=TRUE; case.sens=TRUE; characters.around=0; lines.around=0; lazy=FALSE; clean.up.spaces=TRUE; gen.conc.output=TRUE
   # Thanks to Earl Brown for feedback.

   # if corpus.vector is a factor, convert it to a character string
   if (is.factor(corpus.vector)) { corpus.vector <- as.character(corpus.vector) }

   # make sure that not both characters.around and lines.around are not zero, which would be conflicting information
   if (characters.around!=0 & lines.around!=0) { stop("At least one of 'characters.around' and 'lines.around' has to be zero ...") }

   if (length(search.expression)==2) { # if the user entered 2 search expressions ...
      matches.beg <- gregexpr(search.expression[1], corpus.vector, perl=TRUE) # ... get the positions of the beginnings of the matches
      matches.end <- gregexpr(search.expression[2], corpus.vector, perl=TRUE) # ... get the positions of the ends of the matches
      line.numbers.with.matches <- which(mapply(function(qwe, asd) { qwe[1]==-1 | asd[1]==-1 }, matches.beg, matches.end)==FALSE)
   } else { # if the user entered 1 search expression
      line.numbers.with.matches <- grep(search.expression, corpus.vector, perl=pcre, value=FALSE, ignore.case=!case.sens) # the numbers of lines that contain matches
   }

   proportion.of.corpus.parts.with.matches <- length(line.numbers.with.matches)/length(corpus.vector) # define the range

   if (characters.around!=0 & length(line.numbers.with.matches)>0) { # if the user wants a range of characters around the matches
      building.blocks <- unlist(strsplit(";,<>_-=@#%&ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "")) # define the characters from which the temporary splitter is made
      in.file <- TRUE # set indicator to "yes, the temporary splitter is in the vector to be splitted and can thus not be used"
      while (in.file) { # while the indicator is yes ...
         in.file <- any(grepl(temp <- paste(sample(building.blocks, 10), collapse=""), corpus.vector)) # ... create a new temporary splitter and check whether it's in the vector to be splitted
         lines.with.matches <- gsub(paste("( ?", temp, "|", temp, " ?)", sep=""), " ", # ... 2) replace the 'random' marker with spaces
                                    paste(corpus.vector, collapse = temp), # ... then 1) collapse the corpus vector into one string with a 'random' marker for the line breaks
                                    perl=TRUE) # ... 3) so as to make the whole corpus.vector a one-element character vector
      }
   } else {
      lines.with.matches <- corpus.vector[line.numbers.with.matches] # otherwise, retrieve the lines that contain matches
   }

   if (length(search.expression)==2) { # if the user entered 2 search expressions ...
      if (characters.around!=0) {
         matches.beg <- gregexpr(search.expression[1], lines.with.matches, perl=TRUE, ignore.case=!case.sens)
         matches.end <- gregexpr(search.expression[2], lines.with.matches, perl=TRUE, ignore.case=!case.sens)
      } else {
         matches.beg <- matches.beg[line.numbers.with.matches]
         matches.end <- matches.end[line.numbers.with.matches]
      }
      line.numbers.with.matches.temp <- lines <- starts <- stops <- vector()
      for (current.line.with.matches in seq(lines.with.matches)) {
         pairing.up <- expand.grid(BEGSOFENDS=matches.end[[current.line.with.matches]], BEGSOFBEGS=matches.beg[[current.line.with.matches]]) # pair up all beginning with all endings in this element of corpus.vector, but ...
         pairing.up <- pairing.up[(pairing.up[,1]>pairing.up[,2]),] # ... retain only those where the beginning is before the ending and
         pairing.up <- cbind(ENDSOFENDS=pairing.up$BEGSOFENDS-1+attr(matches.end[[current.line.with.matches]], "match.length")[match(pairing.up$BEGSOFENDS, matches.end[[current.line.with.matches]])], pairing.up) # ... compute the end points of the matches (but retain beginnings for now in case the two search expressions start with the same stuff)
         if (lazy) { # check whether the user wants all matches or just the shortest ...
            pairing.up <- pairing.up[match(unique(pairing.up[,3]), pairing.up[,3]),]
         }
         starts <- c(starts, pairing.up[,3])
         stops <- c(stops, pairing.up[,1])
         lines <- c(lines, rep(lines.with.matches[current.line.with.matches], length(pairing.up[,3])))
         line.numbers.with.matches.temp <- c(line.numbers.with.matches.temp, rep(line.numbers.with.matches[current.line.with.matches], length(pairing.up[,3])))
      }
      line.numbers.with.matches <- line.numbers.with.matches.temp
   } else { # if the user entered 1 search expression ...
      matches <- gregexpr(search.expression, lines.with.matches, perl = pcre, ignore.case = !case.sens) # the start positions and lengths of matches
      number.of.matches <- sapply(matches, length) # the number of matches per line (of the lines that have matches)
      lines <- rep(lines.with.matches, number.of.matches) # the lines with matches, each as many times as it has matches
      line.numbers.with.matches <- rep(line.numbers.with.matches, number.of.matches)
      starts <- unlist(matches) # starting positions of matches
      stops <- starts + unlist(sapply(matches, attr, "match.length")) - 1 # end positions of matches
      # exact.string.matches <- regmatches(corpus.vector, gregexpr(search.expression, corpus.vector, perl=pcre, ignore.case=!case.sens)) # the exact matches
   }
   exact.string.matches <- substr(lines, starts, stops)
   if (length(exact.string.matches)==0) { # if there are no matches ...
      exact.string.matches <- lines.with.delimited.matches <- line.numbers.with.matches <- NULL # set everything to NULL
   } else {
      if (gen.conc.output) { # if the user wants concordance output ...
         lines.with.delimited.matches <- paste( # ... paste together the lines with the tab-delimited matches from ...
            substr(lines, if (characters.around!=0) starts-characters.around else 1, starts-1), "\t", # ... the preceding contexts
            exact.string.matches, "\t", # ... the exact matches
            substr(lines, stops+1, if (characters.around!=0) stops+characters.around else nchar(lines)), # ... and the subsequent contexts
            sep="") # with no delimiter

         if (lines.around!=0) { # if the user wants more lines as contexts ...
            corpus.vector <- append(corpus.vector, rep("", lines.around)) # add to corpus.vector several empty character strings (for easy handling of matches at the end of the corpus)
            starts.of.previous.lines <- pmax(0, line.numbers.with.matches - lines.around) # identify the starting lines of the now longer matches
            ends.of.subsequent.lines <- pmin(line.numbers.with.matches + lines.around, length(corpus.vector)) # identify the ending lines of the now longer matches
            for (current.line.with.delimited.match in seq(lines.with.delimited.matches)) { # for each of the lines with delimited matches ...
               lines.with.delimited.matches[current.line.with.delimited.match] <- paste( # paste together the lines with matches with the larger line-contexts from ...
                  paste(corpus.vector[starts.of.previous.lines[current.line.with.delimited.match]:(line.numbers.with.matches[current.line.with.delimited.match]-1)], collapse=" "), # ... lines preceding the lines with matches
                  lines.with.delimited.matches[current.line.with.delimited.match], # ... the lines with matches
                  paste(corpus.vector[(line.numbers.with.matches[current.line.with.delimited.match]+1):ends.of.subsequent.lines[current.line.with.delimited.match]], collapse=" "), # ... lines following the lines with matches
                  sep=" ") # with a space as a natural delimiter
            }
         }

         # cleaning output as necessary/requested by user
         if (clean.up.spaces) { # if the user wants to have spaces cleaned up ...
            lines.with.delimited.matches <- gsub(" *\t *", "\t", lines.with.delimited.matches, perl=TRUE) # ... clean up spaces around tabs
         }
         lines.with.delimited.matches <- gsub("(^ {1,}| {1,}$)", "", lines.with.delimited.matches, perl=TRUE) # clean up leading and trailing spaces
         if (lines.with.delimited.matches[1]=="\t\t" & length(lines.with.delimited.matches)==1) lines.with.delimited.matches <- NA
      } else { # if the user doesn't want concordance output ...
         lines.with.delimited.matches <- NULL
      }
   }

   # generate output
   output.list <- list(if(!vectorize) { tapply(exact.string.matches, line.numbers.with.matches, c) } else { exact.string.matches },
                       if (characters.around!=0) starts else line.numbers.with.matches, # starting character positions or the numbers of lines with matches, each as many times as it has matches
                       proportion.of.corpus.parts.with.matches,
                       lines.with.delimited.matches,
                       c(Pattern = search.expression, "Corpus (1st 100 char.)"=substr(paste(corpus.vector, collapse=" "), 1, 100), PCRE=pcre, "Case-sensitive"=case.sens, "Version (date)"="2.0 (29 Feb 2016)"))
   names(output.list) <- c("Exact matches",
                           paste("Locations of matches (", ifelse (characters.around!=0, "characters", "lines"), ")", sep=""),
                           "Proportion of non-empty corpus parts with matches",
                           "Lines with delimited matches",
                           "Search parameters and version (date)")
   return(output.list)
}
