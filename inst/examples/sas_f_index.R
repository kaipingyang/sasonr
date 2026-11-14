# INDEX: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Position of the first occurrence of 'bc'.
sas_f_index("abcabc", "bc")  # index('abcabc','bc') = 2

# 0 when the excerpt does not occur.
sas_f_index("abc", "d")  # index('abc','d') = 0

# The search is case-sensitive: 'b' does not match 'B'.
sas_f_index("ABC", "b")  # index('ABC','b') = 0

# A blank excerpt finds the first blank.
sas_f_index("a bc", " ")  # index('a bc',' ') = 2
