# CATT: each call is explained above it and followed by the result of the same call in SAS 9.4

# CATT removes only trailing blanks of each argument; the leading blanks of ' a
# ' and ' c' are kept.
sas_f_catt(" a ", "b ", " c")  # SAS 9.4: catt(' a ', 'b ', ' c') = " ab c"

# The number 3 is converted to character without leading blanks, giving 'x3'.
sas_f_catt("x", 3)  # SAS 9.4: catt('x', 3) = "x3"

# The blank string '' is trimmed to nothing, so the result is 'ab'.
sas_f_catt("a", " ", "b")  # SAS 9.4: catt('a', '', 'b') = "ab"
