# NOTUPPER: each call is explained above it and followed by its result

# The first character that is not an uppercase letter is 'c' at position 3.
sas_f_notupper("ABc")  # notupper('ABc') = 3

# All characters are uppercase letters, so the result is 0.
sas_f_notupper("ABC")  # notupper('ABC') = 0

# Negative start -3 searches leftwards from position 3; the first non-uppercase
# character is 'a' at position 1.
sas_f_notupper("aBC", -3)  # notupper('aBC', -3) = 1
