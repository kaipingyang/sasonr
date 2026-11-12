# CATS: each call is explained above it and followed by the result of the same call in SAS 9.4

# CATS removes leading and trailing blanks from every argument before
# concatenating.
sas_f_cats(" a ", "b ", " c")  # SAS 9.4: cats(' a ', 'b ', ' c') = "abc"

# The number 12.5 is converted to character without blanks, giving 'x12.5'.
sas_f_cats("x", 12.5)  # SAS 9.4: cats('x', 12.5) = "x12.5"

# Blank strings contribute nothing, so the result is blank.
sas_f_cats(" ", " ")  # SAS 9.4: cats('', '') = ""

# A missing numeric argument is converted to '.', giving 'a.b'.
sas_f_cats("a", NA_real_, "b")  # SAS 9.4: cats('a', ., 'b') = "a.b"
