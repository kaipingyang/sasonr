# SQLLIKE: each call is explained above it and followed by its result

# In a LIKE pattern percent matches any sequence of characters, so 'a%' matches
# strings that start with 'a'.
as.numeric(sas_f_sqllike("abc", "a%"))  # 'abc' like 'a%' = 1

# Underscore matches exactly one character, so '_b_' matches any 3-character
# string with 'b' in the middle.
as.numeric(sas_f_sqllike("abc", "_b_"))  # 'abc' like '_b_' = 1

# 'abc' does not start with 'b', so the result is 0.
as.numeric(sas_f_sqllike("abc", "b%"))  # 'abc' like 'b%' = 0

# ESCAPE '\' makes the following percent sign literal, so 'a\%c' matches
# exactly 'a%c'.
as.numeric(sas_f_sqllike("a%c", "a\\%c", "\\"))  # 'a%c' like 'a\%c' escape '\' = 1
