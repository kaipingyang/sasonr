# CHAR: each call is followed by the result of the same call in SAS 9.4
sas_f_char("abcdef", 3)  # SAS: char('abcdef', 3) -> "c"
sas_f_char("abc", 5)  # SAS: char('abc', 5) -> ""
sas_f_char("abc", 1)  # SAS: char('abc', 1) -> "a"
