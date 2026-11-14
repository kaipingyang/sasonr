# CHAR: each call is explained above it and followed by its result

# CHAR returns the single character at position 3, 'c'.
sas_f_char("abcdef", 3)  # char('abcdef', 3) = "c"

# Position 5 is past the end of the string, so the result is blank.
sas_f_char("abc", 5)  # char('abc', 5) = ""

# Position 1 returns the first character, 'a'.
sas_f_char("abc", 1)  # char('abc', 1) = "a"
