# FIRST: each call is followed by the result of the same call in SAS 9.4
sas_f_first("abc")  # SAS: first('abc') -> "a"
sas_f_first("  x")  # SAS: first('  x') -> ""
sas_f_first("")  # SAS: first('') -> ""
