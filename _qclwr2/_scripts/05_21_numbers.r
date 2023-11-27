rm(list=ls(all=TRUE)) # clear memory

# define a vector txt with many different representations of numbers and one test string not to be matched
txt <- c("+1.234567e-4", "+1.234567E-4", "1.234567e-4", "1.234567E-4", "-1.234567e-4", "-1.234567E-4", "+0.12", "0.12", "-0.12", "+.12", ".12", "-.12", "+1.234567e+4", "+1.234567E+4", "1.234567e+4", "1.234567E+4", "-1.234567e+4", "-1.234567E+4", "not this", "+1", "1", "-1", "+123", "123", "-123", "+123.456", "123.456", "-123.456", "+12,345", "12,345", "-12,345", "+12,345.67", "12,345.67", "-12,345.67", "+12345.67", "12345.67", "-12345.67", "+12,345,678.9", "12,345,678.9", "-12,345,678.9")
txt



# compact version
gsub("[+-]?\\.?(\\d+[,.]?)+([eE][+-]\\d+)?", "NUMBER", txt, perl=TRUE)

# commented version (with free-spacing, see <http://www.regular-expressions.info/freespacing.html>).
# That means the regex string can contain spaces, tabs, and newlines,
# which are not processed as search strings, and one can also add comments
gsub(             # replace the matches resulting from the following search string
"(?x)             # set free-spacing
[+-]?             # sign, which is optional
\\.?              # decimal point, which is optional
(                 # a group occurring 1+ times: beginning of group definition
\\d+              #    1+ numbers
[,.]?             #    a thousands separator or a decimal point
)+                # a group occurring 1+ times: end of group definition
([eE][+-]\\d+)?   # exponent, which is optional
",
"NUMBER",         # by the string "NUMBER"
txt,              # in the character vector txt
perl=TRUE)        # using Perl-compatible regular expressions
