# STRIP: each call is followed by the result of the same call in SAS 9.4
sas_f_strip("  a b  ")  # SAS: strip('  a b  ') -> "a b"
sas_f_strip("")  # SAS: strip('') -> ""
sas_f_strip("abc")  # SAS: strip('abc') -> "abc"
