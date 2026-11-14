# COUNTC: each call is explained above it and followed by its result

# COUNTC counts the characters that appear in the list 'an': three a's and two
# n's make 5.
sas_f_countc("banana", "an")  # countc('banana', 'an') = 5

# 'x' does not occur, so the result is 0.
sas_f_countc("abc", "x")  # countc('abc', 'x') = 0

# Modifier i ignores case, so 'b' matches the 'B'.
sas_f_countc("aBc", "b", "i")  # countc('aBc', 'b', 'i') = 1

# Modifier v counts the characters NOT in the list; only 'c' is neither 'a' nor
# 'b', so the result is 1.
sas_f_countc("abc", "ab", "v")  # countc('abc', 'ab', 'v') = 1
