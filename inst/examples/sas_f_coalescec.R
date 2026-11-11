# COALESCEC: each call is followed by the result of the same call in SAS 9.4
sas_f_coalescec("", "b", "c")  # SAS: coalescec('', 'b', 'c') -> "b"
sas_f_coalescec("", " ")  # SAS: coalescec('', ' ') -> ""
sas_f_coalescec(" a", "b")  # SAS: coalescec(' a', 'b') -> " a"
