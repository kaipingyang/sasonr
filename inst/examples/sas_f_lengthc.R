# LENGTHC: each call is explained above it and followed by its result

# LENGTHC counts trailing blanks too: 5.
sas_f_lengthc("abc  ")  # lengthc('abc  ') = 5

# The literal '' is one blank character, so LENGTHC is 1.
sas_f_lengthc(" ")  # lengthc('') = 1

# A single character has length 1.
sas_f_lengthc("a")  # lengthc('a') = 1
