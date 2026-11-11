# SQLINLIST: each call is followed by the result of the same call in SAS 9.4
sas_f_sqlinlist(2, 1, 2, 3)  # SAS: 2 in (1, 2, 3) -> 1
sas_f_sqlinlist("b", "a", "c")  # SAS: 'b' in ('a', 'c') -> 0
sas_f_sqlinlist(NA, NA, 1)  # SAS: . in (., 1) -> 1
