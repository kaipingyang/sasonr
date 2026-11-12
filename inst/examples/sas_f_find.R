# FIND: each call is explained above it and followed by the result of the same call in SAS 9.4

# FIND returns the position of the first occurrence of 'bc', which is 2.
sas_f_find("abcabc", "bc")  # SAS 9.4: find('abcabc', 'bc') = 2

# A character third argument is a modifier: i ignores case, so 'BC' matches at
# position 2.
sas_f_find("abcabc", "BC", "i")  # SAS 9.4: find('abcabc', 'BC', 'i') = 2

# A numeric third argument is the start position: searching rightwards from
# position 3 finds 'bc' at 5.
sas_f_find("abcabc", "bc", 3)  # SAS 9.4: find('abcabc', 'bc', 3) = 5

# Negative start -4 searches leftwards from position 4 and finds the occurrence
# that begins at position 2.
sas_f_find("abcabc", "bc", -4)  # SAS 9.4: find('abcabc', 'bc', -4) = 2

# The substring does not occur, so the result is 0.
sas_f_find("abc", "x")  # SAS 9.4: find('abc', 'x') = 0
