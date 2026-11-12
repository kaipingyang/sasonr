# INDEXC: each call is explained above it and followed by the result of the same call in SAS 9.4

# INDEXC returns the position of the first character that appears in the
# excerpt 'fd': 'd' at position 4.
sas_f_indexc("abcdef", "fd")  # SAS 9.4: indexc('abcdef', 'fd') = 4

# With several excerpts, a character of any of them counts; none of x, y, z
# occurs, so the result is 0.
sas_f_indexc("abc", "x", "yz")  # SAS 9.4: indexc('abc', 'x', 'yz') = 0

# 'c' from the second excerpt occurs at position 3.
sas_f_indexc("abc", "x", "c")  # SAS 9.4: indexc('abc', 'x', 'c') = 3

# INDEXC is case sensitive, so 'b' does not match 'B'.
sas_f_indexc("ABC", "b")  # SAS 9.4: indexc('ABC', 'b') = 0
