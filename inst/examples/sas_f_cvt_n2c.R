# CVT_N2C: each call is explained above it and followed by the result of the same call in SAS 9.4

# Assigning a number to a character variable converts it with the BEST12.
# format, right-aligned in 12 columns.
sas_f_cvt_n2c(3.5)  # SAS 9.4: 3.5 = "         3.5"

# Negative numbers are also right-aligned in 12 columns.
sas_f_cvt_n2c(-12)  # SAS 9.4: -12 = "         -12"

# BEST12. writes a large number such as 10 to the power 15 in E notation, 1E15.
sas_f_cvt_n2c(1e15)  # SAS 9.4: 1e15 = "        1E15"

# BEST12. shows as many decimal digits as fit in 12 columns.
sas_f_cvt_n2c(1/3)  # SAS 9.4: 1/3 = "0.3333333333"
