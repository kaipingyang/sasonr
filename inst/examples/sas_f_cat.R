# CAT: each call is explained above it and followed by its result

# CAT concatenates without removing blanks, so the blanks around ' b ' are kept
# (trailing blanks of the result are not shown).
sas_f_cat("a", " b ", "c ")  # cat('a', ' b ', 'c ') = "a b c"

# A numeric argument is converted with the BEST12. format and its leading
# blanks are removed, giving 'x1'.
sas_f_cat("x", 1)  # cat('x', 1) = "x1"

# The literal '' is one blank character in SAS, and CAT keeps it, so the result
# is 'a b'.
sas_f_cat("a", " ", "b")  # cat('a', '', 'b') = "a b"

# A missing numeric argument is converted to the character '.', giving 'a.b'.
sas_f_cat("a", NA_real_, "b")  # cat('a', ., 'b') = "a.b"
