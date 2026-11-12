# LENGTHM: each call is explained above it and followed by the result of the same call in SAS 9.4

# LENGTHM returns the memory in bytes allocated for the string, including
# trailing blanks: 5.
sas_f_lengthm("abc  ")  # SAS 9.4: lengthm('abc  ') = 5

# The blank literal '' occupies 1 byte.
sas_f_lengthm(" ")  # SAS 9.4: lengthm('') = 1

# A 6-character literal occupies 6 bytes.
sas_f_lengthm("abcdef")  # SAS 9.4: lengthm('abcdef') = 6
