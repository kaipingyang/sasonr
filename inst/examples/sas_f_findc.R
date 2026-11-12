# FINDC: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# FINDC returns the position of the first character that is in the list 'cb':
# 'b' at position 2.
sas_f_findc("abc", "cb")  # SAS 9.4: findc('abc', 'cb') = 2

# No character of the list occurs, so the result is 0.
sas_f_findc("abc", "x")  # SAS 9.4: findc('abc', 'x') = 0

# Modifier i ignores case, so 'B' matches 'b' at position 2.
sas_f_findc("abc", "B", "i")  # SAS 9.4: findc('abc', 'B', 'i') = 2

# Modifier k searches for characters NOT in the list; the first character other
# than 'a' is at position 2.
sas_f_findc("abc", "a", "k")  # SAS 9.4: findc('abc', 'a', 'k') = 2

# Modifier d adds the digits to the (blank) character list, so the digit '1' at
# position 3 is found.
sas_f_findc("ab1", " ", "d")  # SAS 9.4: findc('ab1', '', 'd') = 3
