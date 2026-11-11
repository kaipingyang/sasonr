# TRANSTRN: each call is followed by the result of the same call in SAS 9.4
sas_f_transtrn("banana", "an", "AN")  # SAS: transtrn('banana', 'an', 'AN') -> "bANANa"
sas_f_transtrn("abc", "b", sas_f_trimn(""))  # SAS: transtrn('abc', 'b', trimn('')) -> "ac"
sas_f_transtrn("abc", "x", "y")  # SAS: transtrn('abc', 'x', 'y') -> "abc"
