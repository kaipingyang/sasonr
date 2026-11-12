# QUOTE: each call is explained above it and followed by the result of the same call in SAS 9.4

# QUOTE encloses the value in double quotation marks.
sas_f_quote("abc")  # SAS 9.4: quote('abc') = "\"abc\""

# An embedded double quotation mark is doubled.
sas_f_quote("a\"b")  # SAS 9.4: quote('a"b') = "\"a\"\"b\""

# The optional second argument chooses the quotation mark, here a single quote.
sas_f_quote("abc", "'")  # SAS 9.4: quote('abc', "'") = "'abc'"

# The literal '' is one blank, so the result is a blank enclosed in quotation
# marks.
sas_f_quote(" ")  # SAS 9.4: quote('') = "\" \""
