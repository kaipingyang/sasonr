# FIRST: each call is explained above it and followed by the result of the same call in SAS 9.4

# FIRST returns the first character of the string.
sas_f_first("abc")  # SAS 9.4: first('abc') = "a"

# Leading blanks are not skipped: the first character is a blank, so the result
# is blank.
sas_f_first("  x")  # SAS 9.4: first('  x') = ""

# A blank string gives a blank result.
sas_f_first(" ")  # SAS 9.4: first('') = ""
