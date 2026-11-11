# SQLLIKE: each call is followed by the result of the same call in SAS 9.4
as.numeric(sas_f_sqllike("abc", "a%"))  # SAS: 'abc' like 'a%' -> 1
as.numeric(sas_f_sqllike("abc", "_b_"))  # SAS: 'abc' like '_b_' -> 1
as.numeric(sas_f_sqllike("abc", "b%"))  # SAS: 'abc' like 'b%' -> 0
as.numeric(sas_f_sqllike("a%c", "a\\%c", "\\"))  # SAS: 'a%c' like 'a\%c' escape '\' -> 1
