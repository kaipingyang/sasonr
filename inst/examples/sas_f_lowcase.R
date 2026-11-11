# LOWCASE: each call is followed by the result of the same call in SAS 9.4
sas_f_lowcase("ABC def")  # SAS: lowcase('ABC def') -> "abc def"
sas_f_lowcase("")  # SAS: lowcase('') -> ""
sas_f_lowcase("A1!")  # SAS: lowcase('A1!') -> "a1!"
