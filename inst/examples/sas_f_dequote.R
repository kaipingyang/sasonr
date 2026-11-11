# DEQUOTE: each call is followed by the result of the same call in SAS 9.4
sas_f_dequote("\"abc\"")  # SAS: dequote('"abc"') -> "abc"
sas_f_dequote("'a''b'")  # SAS: dequote("'a''b'") -> "a'b"
sas_f_dequote("abc")  # SAS: dequote('abc') -> "abc"
sas_f_dequote("\"ab\"cd")  # SAS: dequote('"ab"cd') -> "ab"
