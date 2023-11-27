#### Chapter 4: Some statistics for corpus linguistics



### Exercise box 4.1: How would you operationalize these variables? Why? ##########################
# Answer: What follows is not to be interpreted as a set of definite solutions; this is just food for thought.

# (1)   the physical fitness of humans:
# MAXIMALSTRENGTH: How many pounds can a person benchpress twice?
# STRENGTHENDURANCE: How many pounds can a person benchpress 20 times?
# SPEED: How long does a person need to run 100m?
# ENDURANCE: How long can a person run at least 6mph?
# HANDEYECOORDINATION: How many attempts does a person need to hit the exact center of a dart screen 20 times?
# PHYSICALFITNESS: A combined index out of several of the above or other tasks ...



# (2)   the financial wealth of a person:
# the average annual balance on a person's main checking account;
# the average annual balance on all accounts of a person;
# the taxable income at the end of a fiscal year;
# the value of jewelry hidden in a person's pillow;
# the blue book value of someone's car;
# all of the above plus equity from an owned home; ...



# (3)   the givenness/accessibility of a the referent of an NP:
# DISTANCETOLASTMENTION1: the number of clauses up to the previous mention of the same referent with the same lexical element;
# DISTANCETOLASTMENTION2: the number of clauses up to the previous mention of the same referent with the same lexical element or a synonym;
# COHESIVENESS: the number of clauses up to the previous mention of the same referent with the same lexical element or a synonym or a hyperonym;
# TIMESOFPRECEDINGMENTION: the number of times the referent was mentioned in the last 20 clauses ...



# (4)   a person's knowledge of a foreign language:
# the average number of seconds between 2 mistakes in that persons's spoken discourse in the language;
# the average complexity of the sentences that person can form in the language (simple clauses, paratactic sentences, hypotactic sentences, multiple embeddings);
# the grade awarded to a person by an expert rater after a 30-minute interview;
# the average number of errors per 100 words in a text written by that person ...



### Exercise box 4.2: Formulate alternative and null hypotheses ... ###############################
# (1)   to investigate whether 2016 university drop-outs are more likely to be male or female.

# Using counts:
# H1: There number of male university drop-outs differs from that of female university drop-outs; n(male drop-outs in 2016) != n(female drop-outs in 2016).
# H0: The numbers of male and female university drop-outs are the same; n(male drop-outs in 2016) = n(female drop-outs in 2016).

# But this is problematic since it does not take into consideration the overall numbers of male and female students. Thus, one would probably prefer to use a percentage:

# H1: The percentage of male university drop-outs out of all male students is different from the percentage of female university drop-outs out of all female students; %(male drop-outs in 2016) != %(female drop-outs in 2016).
# H0: The percentages of male university drop-outs out of all male students and the percentage of female university drop-outs out of all female students are identical; %(male drop-outs in 2016) = %(female drop-outs in 2016).



# (2)   to investigate whether male or female graduate students complete their Ph.D. degrees differently quickly (in the time period from 1995 to 2016).

# H1: The sum of all quarters needed by male graduate students to complete their Ph.D. degrees (from 1995 to 2016) is different from the sum of all quarters needed by female graduate students (in the same time window); n(male quarters until Ph.D.) != n(female quarters until Ph.D.).
# H0: The sum of all quarters needed by male graduate students to complete their Ph.D. degrees (from 1995 to 2016) is the same as the sum of all quarters needed by female graduate students (in the same time window); n(male quarters until Ph.D.) = n(female quarters until Ph.D.).

# This is problematic for the same reason as above. Thus:

# H1: The average number of quarters needed by male graduate students to complete their Ph.D. degrees (from 1995 to 2016) is different from the average number of all quarters needed by female graduate students (in the same time window); mean(male quarters until Ph.D.) != mean(female quarters until Ph.D.).
# H0: The average number of quarters needed by male graduate students to complete their Ph.D. degrees (from 1995 to 2016) is the same as the average number of all quarters needed by female graduate students (in the same time window); mean(male quarters until Ph.D.) = mean(female quarters until Ph.D.).



### Exercise box 4.3: one-dimensional frequency distributions #####################################
# (1)   A study on relative clauses investigated the frequencies of three types of relative clauses. A small corpus search resulted in 37, 26, and 6 instances of relative clause types RC1, RC2, and RC3 respectively. You want to determine whether your data are distributed in a way that allows you to say they are compatible with a previous study in which the distribution of the relative clause types was 50%, 25%, and 25% respectively.
#     (a) Formulate hypotheses and input the data into R into a vector rcs. Draw a barplot of the data such that the labels used for the three relative clause types are "Relative Clause 1", Relative Clause 2", and "Relative Clause 3"

# H0: The frequencies of the three relative clause types in the corpus are as in the previous corpus study, namely distributed as follows: rc1: 50%, rc2: 25%, rc3: 25%; hence chi-squared=0.
# H1: The frequencies of the three relative clause types in the corpus are not as in the previous corpus study: they are not distributed as follows: rc1: 50%, rc2: 25%, rc3: 25%; hence chi-squared>0

# clear memory
rm(list=ls(all=TRUE))

# enter the data
rcs <- c(37, 26, 6); names(rcs) <- c("RelCl 1", "RelCl 2", "RelCl 3")

# generate a barplot and costomize the colors of the bars (and omit the spacing between bars)
temp <- barplot(rcs, col=c("blue", "red", "green"), space=0)
   text(temp,  # plot text at the x-coordinates of the middle of the bars that are stored in temp now
        rcs/2, # and at the y coordinates that correspond to half the heights of the bars
        rcs)   # namely text that corresponds to the observed frequencies



#     (b) Compute a chi-squared test for goodness of fit to determine whether your results are compatible with a previous study that found that the first relative clause type accounted for half of all whereas the other two were equally frequent.
# compute a chi-squared test for goodness of fit
(rcs.test <- chisq.test(rcs, p=c(0.5, 0.25, 0.25), correct=FALSE))
rcs.test$expected
rcs.test$residuals

# The data show that the null hypothesis can be rejected: Chi-squared=11.96, df=2, p=0.0025. In the present data, rc type 1 is just about as frequent as would be expected from the previous study, but rc types 2 and 3 are over and underrepresented in the current data respectively.



### Exercise box 4.4: two-dimensional frequency distributions #####################################
# (1)   In a (fictitious) corpus analysis, the disfluency markers uh and uhm were observed before content words and function words. The data in <_qclwr2/_inputfiles/stat_disfluencies.csv> were obtained. Load the data into a data frame disfluencies and analyze them graphically and statistically (compute both a significance test and an effect size).

# clear memory
rm(list=ls(all=TRUE))

# H0: The frequencies of "uh" and "uhm" do not correlate with the category of the following word (content word vs. function word); chi-squared=0.
# H1: The frequencies of "uh" and "uhm" correlate with the categories of the following word (content word vs. function word); chi-squared>0.

disfluencies <- read.table(file.choose(), header=TRUE, sep="\t", comment.char="") # load the data
attach(disfluencies); str(disfluencies) # make the variable names available; checking the input

table(DISFLUENCY, LOCATION)
mosaicplot(t(table(DISFLUENCY, LOCATION)),
           main="The relation between DISFLUENCY and LOCATION")

(disfl.test <- chisq.test(table(DISFLUENCY, LOCATION), correct=FALSE))
disfl.test$expected # compute the expected frequencies
disfl.test$residuals # inspect the residuals to see where the effect comes from

# compute an effect size: Cramer's V
numerator <- disfl.test$statistic
denominator <- sum(table(DISFLUENCY, LOCATION))*(min(dim(table(DISFLUENCY, LOCATION)))-1)
fraction <- numerator/denominator
(phi <- sqrt(fraction))

assocplot(t(table(DISFLUENCY, LOCATION))) # inspect an association plot to see where the effect comes from

# The analysis shows that "uh" is more frequent than expected before function words and less frequent than expected before content words whereas "uhm" is less frequent than expected before function words and more frequent than expected before content words. This tencency is intermediately strong (Cramer's V=0.346) and statistically highly significant (chi=square=12.448; df=1; p<0.001), which is why the null hypothesis can be rejected.



# (2)   In a (fictitious) corpus analysis, the frequencies of "different from", "different than", and "different to" in British English and American English were compared. The data in <_qclwr2/_inputfiles/stat_different.csv> were obtained. Load the data into a data frame different and analyze them graphically and statistically (compute both a significance test and an effect size).

# clear memory
rm(list=ls(all=TRUE))

# H0: The frequencies of "from", "than", and "to" do not correlate with the variety of English (BrE vs. AmE); chi-squared=0.
# H1: The frequencies of "from", "than", and "to" correlate with the variety of English (BrE vs. AmE); chi-squared>0.

different<-read.table(file.choose(), header=TRUE, sep="\t", comment.char="") # load the data
str(different); attach(different) # check the input; make the variable names available

table(VARIETY, COLLOCATE)
mosaicplot(t(table(VARIETY, COLLOCATE)),
           main="The relation between VARIETY and COLLOCATE")

(diff.test <- chisq.test(table(VARIETY, COLLOCATE), correct=FALSE))
diff.test$expected # compute the expected frequencies - WARNING: two of the expected freqs are smaller than 5: see below
diff.test$residuals # inspect the residuals to see where the effect comes from

# compute an effect size: Cramer's V
numerator <- diff.test$statistic
denominator <- sum(table(VARIETY, COLLOCATE))*(min(dim(table(VARIETY)))-1)
fraction <- numerator/denominator
(phi <- sqrt(fraction))

assocplot(t(table(VARIETY, COLLOCATE))) # inspect an association plot to see where the effect comes from

# but the assumptions for the chi-squared test were not met so, to be safe, we compute an exact test as well:
fisher.test(table(VARIETY, COLLOCATE))

# The analysis shows that, in American English, "different than" is more frequent than expected while "different from" and "different to" are less frequent than expected. In British English, the tendency is reversed: "different from" and "different to" are more frequent than expected and "different than" is less frequent than expected. This tencency is intermediately strong (Cramer's V=0.325) and statistically highly significant both according to a chi-squared test for independence (chi=squared=21.068; df=2; p<0.001) and a Fisher-Yates exact test (p<0.0001), which is why the null hypothesis can be rejected.



# (3)   In a fictitious corpus analysis on gender stereotypes, a researcher collected all occurrences of "he" or "she" directly followed by either "loves" or "hates". She obtained the data provided in <_qclwr2/_inputfiles/stat_love-and-hate.csv>. Load the data into a data frame lovehate and analyze them graphically and statistically (compute both a significance test and an effect size).

# clear memory
rm(list=ls(all=TRUE))

# H0: The proportions of "loves", and "hates" after "he" and "she" are identical; chi-squared>0.
# H1: The proportions of "loves", and "hates" after "he" and "she" are not identical; chi-squared>0.

lovehate<-read.table(file.choose(), header=TRUE, sep="\t", comment.char="") # load the date
str(lovehate); attach(lovehate) # check the input; make the variable names available

table(PRONOUN, VERB)
mosaicplot(t(table(PRONOUN, VERB)),
           main="The relation between PRONOUN and VERB", col=c("blue", "pink"))

(loha.test <- chisq.test(table(PRONOUN, VERB), correct=FALSE))
loha.test$expected # compute the expected frequencies
loha.test$residuals # inspect the residuals to see where the effect comes from

# compute an effect size: Cramer's V
numerator <- loha.test$statistic
denominator <- sum(table(PRONOUN, VERB))*(min(dim(table(PRONOUN)))-1)
fraction <- numerator/denominator
(phi <- sqrt(fraction))

assocplot(t(table(PRONOUN, VERB))) # inspect an association plot to see where the effect comes from

# The analysis shows that "he loves" and "she hates are mpore frequent than expected whereas "he hates" and "she loves" are less frequent than expected. However, this tendency is rather weak (phi=0.131) and, thus, statistically not significant (chi-squared=1.99; df=1; p=0.158), which is why the null hypothesis must not be rejected.



# (4)   Ihalainen (1991) investigates the frequencies of different explicitly marked habitual past tense forms in East and West Somerset dialects and reports the following distribution:
# Dialect    Did    Used to    Would    Totals
# East        20         36       16        72
# West         2         48       33        83
# Totals      22         84       49       155

# While Ihalainen reports the results of a chi-squared test for the overall frequencies of East and West Somerset dialect (72 vs. 83), he does not report the results of a chi-squared test for the overall table. Perform a chi-squared test for these data and discuss the results.

# clear memory
rm(list=ls(all=TRUE))

ihalainen <- matrix(c(20, 36, 16, 2, 48, 33),
                    byrow=TRUE, ncol=3,
                    dimnames=list(DIALECT=c("east", "west"), PASTFORMS=c("did", "used to", "would")))
ihalainen
mosaicplot(t(ihalainen))

(iha.test<-chisq.test(ihalainen, correct=FALSE))
iha.test$expected # compute the expected frequencies
iha.test$residuals # inspect the residuals to see where the effect comes from

# compute an effect size: Cramer's V
numerator<-iha.test$statistic
denominator<-sum(ihalainen)*(min(dim(ihalainen))-1)
fraction<-numerator/denominator
(phi<-sqrt(fraction))

assocplot(t(ihalainen))

# The data show that there is a significant interaction between the dialect area and the explicitly marked habitual past tense forms: chi-squared=21.668; df = 2, p=1.972e-05. While the overall effect is intermediately strong (Cramer's V=0.374), not all cells contribute to this effect equally: The by far strongest effect is observed for "did", which is about twice as frequent in East Somerset dialect than expected while "did" occurs in West Somerset dialect only about one sixth of the time it is expected. "would", on the other hand, is preferred in West Somerset dialect, occurring there about 20% of the time more often than expected. Finally, "used to" does not exhibit a strong preference in either dialect area.



### Exercise box 4.5: averages ####################################################################
# In a (fictitious) corpus analysis, the morphemic lengths of the NPs expressing recipients in ditransitive constructions (e.g. "He gave _him_ the book") and prepositional dative constructions (e.g., "He sent it to _his dad_") were recorded. The data in <_qclwr2/_inputfiles/stat_recipients.csv> were obtained. Formulate the alternative hypotheses and null hypotheses in text form and in statistical form, load the data into a data frame recipients, and analyze them graphically and statistically.

# clear memory
rm(list=ls(all=TRUE))

# H0: The recipients are equally long in both constructions; mean(length of recipient in ditransitives) = mean(length of recipient in prepositional datives).
# H1: The recipients are not equally long in both constructions; mean(length of recipient in ditransitives) != mean(length of recipient in prepositional datives).

recipients<-read.table(file.choose(), header=TRUE, sep="\t", comment.char="") # load the data
str(recipients); attach(recipients)

boxplot(LENGTH_IO~CONSTRUCTION, notch=TRUE); grid()

tapply(LENGTH_IO, CONSTRUCTION, mean) # compute the means
tapply(LENGTH_IO, CONSTRUCTION, sd)   # compute the standard deviations

# look up ?shapiro.test: it's a very simple function just requiring a numeric vector as its only argument
tapply(LENGTH_IO, CONSTRUCTION, shapiro.test) # both distributions do not differ significantly from normality

tapply(LENGTH_IO, CONSTRUCTION, var) # compute the variances

# this test I didn't discuss in the book (see the references mentioned there):
# it tests whether two variances are significantly different from each other
var.test(LENGTH_IO~CONSTRUCTION)     # the variances do not differ significantly from each other

# both conditions are met: we are allowed to compute the t-test.
t.test(LENGTH_IO~CONSTRUCTION)

# The average morphemic length of recipients in ditransitives is 6.27 morphemes (standard deviation = 1.95) while the average morphemic length of recipients in prepositional datives is 5.55 morphemes (standard deviation = 2.42). According to a t-test, this difference of 0.72 morphemes is statistically not significant (t=1.568; df=71.28; p=0.1213), which is why the null hypothesis must not be rejected.
