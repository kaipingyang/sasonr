# CHOOSEC: each call is followed by the result of the same call in SAS 9.4
sas_f_choosec(2, "a", "b", "c")  # SAS: choosec(2, 'a', 'b', 'c') -> "b"
sas_f_choosec(-1, "a", "b", "c")  # SAS: choosec(-1, 'a', 'b', 'c') -> "c"
sas_f_choosec(4, "a", "b")  # SAS: choosec(4, 'a', 'b') -> ""
