# LENGTHN: each call is explained above it and followed by its result

# LENGTHN returns the length without trailing blanks: 3.
sas_f_lengthn("abc  ")  # lengthn('abc  ') = 3

# Unlike LENGTH, LENGTHN returns 0 for a blank string.
sas_f_lengthn(" ")  # lengthn('') = 0

# Leading blanks are counted.
sas_f_lengthn("  a")  # lengthn('  a') = 3
