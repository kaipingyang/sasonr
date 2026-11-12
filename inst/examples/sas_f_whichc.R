# WHICHC: each call is explained above it and followed by the result of the same call in SAS 9.4

# WHICHC returns the index of the first value equal to the first argument: 'b'
# is the 2nd value.
sas_f_whichc("b", "a", "b", "c")  # SAS 9.4: whichc('b', 'a', 'b', 'c') = 2

# No value matches, so the result is 0.
sas_f_whichc("x", "a", "b")  # SAS 9.4: whichc('x', 'a', 'b') = 0

# Trailing blanks are ignored in the comparison, so 'a ' equals 'a'.
sas_f_whichc("a ", "a")  # SAS 9.4: whichc('a ', 'a') = 1
