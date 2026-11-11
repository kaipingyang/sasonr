# UPCASE: each call is followed by the result of the same call in SAS 9.4
sas_f_upcase("abc Def")  # SAS: upcase('abc Def') -> "ABC DEF"
sas_f_upcase("")  # SAS: upcase('') -> ""
sas_f_upcase("a1!")  # SAS: upcase('a1!') -> "A1!"
