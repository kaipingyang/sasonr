# NVALID: each call is followed by the result of the same call in SAS 9.4
sas_f_nvalid("abc")  # SAS: nvalid('abc') -> 1
sas_f_nvalid("1abc")  # SAS: nvalid('1abc') -> 0
sas_f_nvalid("my var", "v7")  # SAS: nvalid('my var', 'v7') -> 0
sas_f_nvalid("my var", "any")  # SAS: nvalid('my var', 'any') -> 1
sas_f_nvalid("_x1", "nliteral")  # SAS: nvalid('_x1', 'nliteral') -> 1
