# LENGTHM: each call is explained above it and followed by its result

# LENGTHM returns the memory in bytes allocated for the string, including
# trailing blanks: 5.
sas_f_lengthm("abc  ")  # lengthm('abc  ') = 5

# The blank literal '' occupies 1 byte.
sas_f_lengthm(" ")  # lengthm('') = 1

# A 6-character literal occupies 6 bytes.
sas_f_lengthm("abcdef")  # lengthm('abcdef') = 6
