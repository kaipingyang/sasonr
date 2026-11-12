# NOTUPPER: each call is explained above it and followed by the result of the same call in SAS 9.4

# The first character that is not an uppercase letter is 'c' at position 3.
sas_f_notupper("ABc")  # SAS 9.4: notupper('ABc') = 3

# All characters are uppercase letters, so the result is 0.
sas_f_notupper("ABC")  # SAS 9.4: notupper('ABC') = 0

# Negative start -3 searches leftwards from position 3; the first non-uppercase
# character is 'a' at position 1.
sas_f_notupper("aBC", -3)  # SAS 9.4: notupper('aBC', -3) = 1
