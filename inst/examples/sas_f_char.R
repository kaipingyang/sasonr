# CHAR: each call is explained above it and followed by the result of the same call in SAS 9.4

# CHAR returns the single character at position 3, 'c'.
sas_f_char("abcdef", 3)  # SAS 9.4: char('abcdef', 3) = "c"

# Position 5 is past the end of the string, so the result is blank.
sas_f_char("abc", 5)  # SAS 9.4: char('abc', 5) = ""

# Position 1 returns the first character, 'a'.
sas_f_char("abc", 1)  # SAS 9.4: char('abc', 1) = "a"
