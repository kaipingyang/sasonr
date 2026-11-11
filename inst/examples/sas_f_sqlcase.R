# SQLCASE: each call is followed by the result of the same call in SAS 9.4
sas_f_sqlcase(2 > 1, "yes", "no")  # SAS: case when 2 > 1 then 'yes' else 'no' end -> "yes"
sas_f_sqlcase(1 > 2, 1, 2 > 1, 2, 3)  # SAS: case when 1 > 2 then 1 when 2 > 1 then 2 else 3 end -> 2
sas_f_sqlcase(1 > 2, 1, 3)  # SAS: case when 1 > 2 then 1 else 3 end -> 3
