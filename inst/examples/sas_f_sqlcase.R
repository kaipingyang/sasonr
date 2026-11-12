# SQLCASE: each call is explained above it and followed by the result of the same call in SAS 9.4

# A CASE expression returns the value of the first WHEN clause whose condition
# is true.
sas_f_sqlcase(2 > 1, "yes", "no")  # SAS 9.4: case when 2 > 1 then 'yes' else 'no' end = "yes"

# The first condition is false and the second is true, so the result is 2.
sas_f_sqlcase(1 > 2, 1, 2 > 1, 2, 3)  # SAS 9.4: case when 1 > 2 then 1 when 2 > 1 then 2 else 3 end = 2

# No WHEN condition is true, so the ELSE value 3 is returned.
sas_f_sqlcase(1 > 2, 1, 3)  # SAS 9.4: case when 1 > 2 then 1 else 3 end = 3
