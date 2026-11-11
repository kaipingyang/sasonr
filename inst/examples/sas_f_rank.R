# RANK: each call is followed by the result of the same call in SAS 9.4
sas_f_rank("A")  # SAS: rank('A') -> 65
sas_f_rank("a")  # SAS: rank('a') -> 97
sas_f_rank(" ")  # SAS: rank(' ') -> 32
sas_f_rank("0")  # SAS: rank('0') -> 48
