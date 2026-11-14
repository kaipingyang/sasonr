# FIRST: each call is explained above it and followed by its result

# FIRST returns the first character of the string.
sas_f_first("abc")  # first('abc') = "a"

# Leading blanks are not skipped: the first character is a blank, so the result
# is blank.
sas_f_first("  x")  # first('  x') = ""

# A blank string gives a blank result.
sas_f_first(" ")  # first('') = ""
