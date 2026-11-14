# SCAN: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# The first word (default delimiters include the blank).
sas_f_scan("a b c", 1)  # scan('a b c',1) = "a"

# A negative count counts words from the end: -1 is the last word.
sas_f_scan("a b c", -1)  # scan('a b c',-1) = "c"

# A count beyond the number of words gives a blank.
sas_f_scan("a b c", 4)  # scan('a b c',4) = ""

# Consecutive delimiters count as one, so the third word is 'c'.
sas_f_scan("a,b,,c", 3)  # scan('a,b,,c',3) = "c"
