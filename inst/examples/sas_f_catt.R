# CATT: each call is explained above it and followed by its result

# CATT removes only trailing blanks of each argument; the leading blanks of ' a
# ' and ' c' are kept.
sas_f_catt(" a ", "b ", " c")  # catt(' a ', 'b ', ' c') = " ab c"

# The number 3 is converted to character without leading blanks, giving 'x3'.
sas_f_catt("x", 3)  # catt('x', 3) = "x3"

# The blank string '' is trimmed to nothing, so the result is 'ab'.
sas_f_catt("a", " ", "b")  # catt('a', '', 'b') = "ab"
