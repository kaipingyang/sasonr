# SUBSTRN: each call is followed by the result of the same call in SAS 9.4
sas_f_substrn("abcdef", 2, 3)  # SAS: substrn('abcdef', 2, 3) -> "bcd"
sas_f_substrn("abcdef", 0, 3)  # SAS: substrn('abcdef', 0, 3) -> "ab"
sas_f_substrn("abcdef", 5, 10)  # SAS: substrn('abcdef', 5, 10) -> "ef"
sas_f_substrn("abc", 4)  # SAS: substrn('abc', 4) -> ""
