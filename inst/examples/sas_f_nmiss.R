# NMISS: each call is explained above it and followed by its result

# NMISS counts the missing numeric arguments: 1.
sas_f_nmiss(1, NA_real_, 3)  # nmiss(1, ., 3) = 1

# The special missing values .A and ._ count as missing, 0 does not: 2.
sas_f_nmiss(sas_missing(".A"), sas_missing("._"), 0)  # nmiss(.A, ._, 0) = 2

# No argument is missing, so the count is 0.
sas_f_nmiss(1, 2)  # nmiss(1, 2) = 0
