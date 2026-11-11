# CATS: each call is followed by the result of the same call in SAS 9.4
sas_f_cats(" a ", "b ", " c")  # SAS: cats(' a ', 'b ', ' c') -> "abc"
sas_f_cats("x", 12.5)  # SAS: cats('x', 12.5) -> "x12.5"
sas_f_cats("", "")  # SAS: cats('', '') -> ""
sas_f_cats("a", NA_real_, "b")  # SAS: cats('a', ., 'b') -> "a.b"
