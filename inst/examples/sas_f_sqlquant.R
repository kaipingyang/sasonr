# SQLQUANT: each call is followed by the result of the same call in SAS 9.4
sas_f_sqlquant(1, c(2, 3), "<", "any")  # SAS: 1 < any (select v from work.t) -> 1
sas_f_sqlquant(3, c(2, 3), ">", "all")  # SAS: 3 > all (select v from work.t) -> 0
sas_f_sqlquant(3, c(2, 3), ">=", "all")  # SAS: 3 >= all (select v from work.t) -> 1
