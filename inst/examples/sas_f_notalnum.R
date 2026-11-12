# NOTALNUM: each call is explained above it and followed by the result of the same call in SAS 9.4

# NOTALNUM returns the first position of a character that is neither a letter
# nor a digit: '!' at 4.
sas_f_notalnum("ab1!")  # SAS 9.4: notalnum('ab1!') = 4

# All characters are letters or digits, so the result is 0.
sas_f_notalnum("abc")  # SAS 9.4: notalnum('abc') = 0

# Negative start -3 searches leftwards from position 3 ('b') and finds '!' at
# position 2.
sas_f_notalnum("a!b", -3)  # SAS 9.4: notalnum('a!b', -3) = 2
