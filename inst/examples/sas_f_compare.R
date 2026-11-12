# COMPARE: each call is explained above it and followed by the result of the same call in SAS 9.4

# COMPARE returns 0 when the two strings are equal.
sas_f_compare("abc", "abc")  # SAS 9.4: compare('abc', 'abc') = 0

# The strings first differ at position 3; the sign is negative because 'c'
# sorts before 'd', so -3.
sas_f_compare("abc", "abd")  # SAS 9.4: compare('abc', 'abd') = -3

# The first difference is at position 3 and 'd' sorts after 'c', so the result
# is +3.
sas_f_compare("abd", "abc")  # SAS 9.4: compare('abd', 'abc') = 3

# Modifier i ignores case, so 'ABC' and 'abc' compare equal (0).
sas_f_compare("ABC", "abc", "i")  # SAS 9.4: compare('ABC', 'abc', 'i') = 0

# The shorter string is padded with blanks; 'c' sorts after a blank, so the
# result is +3 (first difference at 3).
sas_f_compare("abc", "ab")  # SAS 9.4: compare('abc', 'ab') = 3

# Modifier : truncates the longer string to the length of the shorter one, so
# 'ab' and 'abc' compare equal.
sas_f_compare("ab", "abc", ":")  # SAS 9.4: compare('ab', 'abc', ':') = 0
