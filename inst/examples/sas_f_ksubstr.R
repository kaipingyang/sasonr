# KSUBSTR: each call is explained above it and followed by its result

# Three characters from position 2.
sas_f_ksubstr("abcdef", 2, 3)  # ksubstr('abcdef',2,3) = "bcd"

# For ASCII values KSUBSTR is SUBSTR: a position beyond the end is invalid
# (blank result, NOTE and _ERROR_ = 1 in a DATA step).
sas_f_ksubstr("abcdef", 7)  # ksubstr('abcdef',7) = ""
