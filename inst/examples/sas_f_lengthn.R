# LENGTHN: each call is explained above it and followed by the result of the same call in SAS 9.4

# LENGTHN returns the length without trailing blanks: 3.
sas_f_lengthn("abc  ")  # SAS 9.4: lengthn('abc  ') = 3

# Unlike LENGTH, LENGTHN returns 0 for a blank string.
sas_f_lengthn(" ")  # SAS 9.4: lengthn('') = 0

# Leading blanks are counted.
sas_f_lengthn("  a")  # SAS 9.4: lengthn('  a') = 3
