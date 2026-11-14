# LENGTH: each call is explained above it and followed by its result

# LENGTH returns the length without trailing blanks: 3.
sas_f_length("abc  ")  # length('abc  ') = 3

# For a blank string LENGTH returns 1, not 0.
sas_f_length(" ")  # length('') = 1

# Leading blanks are counted.
sas_f_length("  a")  # length('  a') = 3

# A numeric argument is converted with BEST12. to a 12-character string, so the
# length is 12.
sas_f_length(123)  # length(123) = 12
