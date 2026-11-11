# COMPBL: each call is followed by the result of the same call in SAS 9.4
sas_f_compbl("a   b    c")  # SAS: compbl('a   b    c') -> "a b c"
sas_f_compbl("  a  b")  # SAS: compbl('  a  b') -> " a b"
sas_f_compbl("a")  # SAS: compbl('a') -> "a"
