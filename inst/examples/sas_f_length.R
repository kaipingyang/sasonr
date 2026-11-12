# LENGTH: each call is explained above it and followed by the result of the same call in SAS 9.4

# LENGTH returns the length without trailing blanks: 3.
sas_f_length("abc  ")  # SAS 9.4: length('abc  ') = 3

# For a blank string LENGTH returns 1, not 0.
sas_f_length(" ")  # SAS 9.4: length('') = 1

# Leading blanks are counted.
sas_f_length("  a")  # SAS 9.4: length('  a') = 3

# A numeric argument is converted with BEST12. to a 12-character string, so the
# length is 12.
sas_f_length(123)  # SAS 9.4: length(123) = 12
