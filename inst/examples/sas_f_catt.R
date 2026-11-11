# CATT: each call is followed by the result of the same call in SAS 9.4
sas_f_catt(" a ", "b ", " c")  # SAS: catt(' a ', 'b ', ' c') -> " ab c"
sas_f_catt("a", "", "b")  # SAS: catt('a', '', 'b') -> "ab"
