# LENGTHC: each call is explained above it and followed by the result of the same call in SAS 9.4

# LENGTHC counts trailing blanks too: 5.
sas_f_lengthc("abc  ")  # SAS 9.4: lengthc('abc  ') = 5

# The literal '' is one blank character, so LENGTHC is 1.
sas_f_lengthc(" ")  # SAS 9.4: lengthc('') = 1

# A single character has length 1.
sas_f_lengthc("a")  # SAS 9.4: lengthc('a') = 1
