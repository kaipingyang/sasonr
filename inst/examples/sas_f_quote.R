# QUOTE: each call is explained above it and followed by its result

# QUOTE encloses the value in double quotation marks.
sas_f_quote("abc")  # quote('abc') = "\"abc\""

# An embedded double quotation mark is doubled.
sas_f_quote("a\"b")  # quote('a"b') = "\"a\"\"b\""

# The optional second argument chooses the quotation mark, here a single quote.
sas_f_quote("abc", "'")  # quote('abc', "'") = "'abc'"

# The literal '' is one blank, so the result is a blank enclosed in quotation
# marks.
sas_f_quote(" ")  # quote('') = "\" \""
