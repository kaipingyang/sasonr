# FACT: each call is followed by the result of the same call in SAS 9.4
sas_f_fact(5)  # SAS: fact(5) -> 120
sas_f_fact(0)  # SAS: fact(0) -> 1
sas_f_fact(20)  # SAS: fact(20) -> 2.43290200817664e+18
sas_f_fact(NA_real_)  # SAS: fact(.) -> .
