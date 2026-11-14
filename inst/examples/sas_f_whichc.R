# WHICHC: each call is explained above it and followed by its result

# WHICHC returns the index of the first value equal to the first argument: 'b'
# is the 2nd value.
sas_f_whichc("b", "a", "b", "c")  # whichc('b', 'a', 'b', 'c') = 2

# No value matches, so the result is 0.
sas_f_whichc("x", "a", "b")  # whichc('x', 'a', 'b') = 0

# Trailing blanks are ignored in the comparison, so 'a ' equals 'a'.
sas_f_whichc("a ", "a")  # whichc('a ', 'a') = 1
