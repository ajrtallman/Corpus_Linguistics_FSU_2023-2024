#### Chapter 4: Some statistics for corpus linguistics



### Exercise box 4.1: How would you operationalize these variables? Why? ##########################
# (1)   the physical fitness of humans:



# (2)   the financial wealth of a person:



# (3)   the givenness/accessibility of a the referent of an NP:



# (4)   a person's knowledge of a foreign language:



### Exercise box 4.2: Formulate alternative and null hypotheses ... ###############################
# (1)   to investigate whether 2016 university drop-outs are more likely to be male or female.



# (2)   to investigate whether male or female graduate students complete their Ph.D. degrees differently quickly (in the time period frm 1995 to 2016).




### Exercise box 4.3: one-dimensional frequency distributions #####################################
# (1)   A study on relative clauses investigated the frequencies of three types of relative clauses. A small corpus search resulted in 37, 26, and 6 instances of relative clause types RC1, RC2, and RC3 respectively. You want to determine whether your data are distributed in a way that allows you to say they are compatible with a previous study in which the distribution of the relative clause types was 50%, 25%, and 25% respectively.
#     (a) Formulate hypotheses and input the data into R into a vector rcs. Draw a barplot of the data such that the labels used for the three relative clause types are "Relative Clause 1", Relative Clause 2", and "Relative Clause 3"



#     (b) Compute a chi-squared test for goodness of fit to determine whether your results are compatible with a previous study that found that the first relative clause type accounted for half of all whereas the other two were equally frequent.
# compute a chi-squared test for goodness of fit



### Exercise box 4.4: two-dimensional frequency distributions #####################################
# (1)   In a (fictitious) corpus analysis, the disfluency markers uh and uhm were observed before content words and function words. The data in <_qclwr2/_inputfiles/stat_disfluencies.csv> were obtained. Load the data into a data frame disfluencies and analyze them graphically and statistically (compute both a significance test and an effect size).



# (2)   In a (fictitious) corpus analysis, the frequencies of "different from", "different than", and "different to" in British English and American English were compared. The data in <_qclwr2/_inputfiles/stat_different.csv> were obtained. Load the data into a data frame different and analyze them graphically and statistically (compute both a significance test and an effect size).



# (3)   In a fictitious corpus analysis on gender stereotypes, a researcher collected all occurrences of "he" or "she" directly followed by either "loves" or "hates". She obtained the data provided in <_qclwr2/_inputfiles/stat_love-and-hate.csv>. Load the data into a data frame lovehate and analyze them graphically and statistically (compute both a significance test and an effect size).



# (4)   Ihalainen (1991) investigates the frequencies of different explicitly marked habitual past tense forms in East and West Somerset dialects and reports the following distribution:
# Dialect    Did    Used to    Would    Totals
# East        20         36       16        72
# West         2         48       33        83
# Totals      22         84       49       155

# While Ihalainen reports the results of a chi-squared test for the overall frequencies of East and West Somerset dialect (72 vs. 83), he does not report the results of a chi-squared test for the overall table. Perform a chi-squared test for these data and discuss the results.



### Exercise box 4.5: averages ####################################################################
# In a (fictitious) corpus analysis, the morphemic lengths of the NPs expressing recipients in ditransitive constructions (e.g. "He gave _him_ the book") and prepositional dative constructions (e.g., "He sent it to _his dad_") were recorded. The data in <_qclwr2/_inputfiles/stat_recipients.csv> were obtained. Formulate the alternative hypotheses and null hypotheses in text form and in statistical form, load the data into a data frame recipients, and analyze them graphically and statistically.
