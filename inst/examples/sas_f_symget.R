# SYMGET: each call is followed by the result of the same call in SAS 9.4
sas_f_symget(list2env(list(mv = "hello")), "mv")  # SAS: symget('mv') -> "hello"
sas_f_symget(list2env(list(mv = "hello")), "MV")  # SAS: symget('MV') -> "hello"
