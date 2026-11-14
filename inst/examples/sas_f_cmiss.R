# CMISS: each call is explained above it and followed by its result

# CMISS counts missing numeric or character arguments: . and the blank string
# '' are both missing, giving 2.
sas_f_cmiss(1, NA_real_, "a", " ")  # cmiss(1, ., 'a', '') = 2

# A special missing value such as .A counts as missing.
sas_f_cmiss(sas_missing(".A"))  # cmiss(.A) = 1

# Neither 'x' nor 2 is missing, so the count is 0.
sas_f_cmiss("x", 2)  # cmiss('x', 2) = 0
