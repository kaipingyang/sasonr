# SQLLIKE: each call is explained above it and followed by the result of the same call in SAS 9.4

# In a LIKE pattern percent matches any sequence of characters, so 'a%' matches
# strings that start with 'a'.
as.numeric(sas_f_sqllike("abc", "a%"))  # SAS 9.4: 'abc' like 'a%' = 1

# Underscore matches exactly one character, so '_b_' matches any 3-character
# string with 'b' in the middle.
as.numeric(sas_f_sqllike("abc", "_b_"))  # SAS 9.4: 'abc' like '_b_' = 1

# 'abc' does not start with 'b', so the result is 0.
as.numeric(sas_f_sqllike("abc", "b%"))  # SAS 9.4: 'abc' like 'b%' = 0

# ESCAPE '\' makes the following percent sign literal, so 'a\%c' matches
# exactly 'a%c'.
as.numeric(sas_f_sqllike("a%c", "a\\%c", "\\"))  # SAS 9.4: 'a%c' like 'a\%c' escape '\' = 1
