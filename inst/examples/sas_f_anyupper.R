# ANYUPPER: each call is followed by the result of the same call in SAS 9.4
sas_f_anyupper("abC")  # SAS: anyupper('abC') -> 3
sas_f_anyupper("abc")  # SAS: anyupper('abc') -> 0
sas_f_anyupper("AbC", -2)  # SAS: anyupper('AbC', -2) -> 1
