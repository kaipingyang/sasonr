# INDEX: each call is explained above it and followed by the result of the same call in SAS 9.4
# (dates are SAS day counts from 1960-01-01)

# Position of the first occurrence of 'bc'.
sas_f_index("abcabc", "bc")  # SAS 9.4: index('abcabc','bc') = 2

# 0 when the excerpt does not occur.
sas_f_index("abc", "d")  # SAS 9.4: index('abc','d') = 0

# The search is case-sensitive: 'b' does not match 'B'.
sas_f_index("ABC", "b")  # SAS 9.4: index('ABC','b') = 0

# A blank excerpt finds the first blank.
sas_f_index("a bc", " ")  # SAS 9.4: index('a bc',' ') = 2
