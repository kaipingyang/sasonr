# SQLQUANT: each call is explained above it and followed by the result of the same call in SAS 9.4

# ANY is true if the comparison holds for at least one value of the subquery: 1
# < 2 holds.
sas_f_sqlquant(1, c(2, 3), "<", "any")  # SAS 9.4: 1 < any (select v from work.t) = 1

# ALL requires the comparison to hold for every value; 3 > 3 is false, so the
# result is 0.
sas_f_sqlquant(3, c(2, 3), ">", "all")  # SAS 9.4: 3 > all (select v from work.t) = 0

# 3 >= 2 and 3 >= 3 both hold, so the result is 1.
sas_f_sqlquant(3, c(2, 3), ">=", "all")  # SAS 9.4: 3 >= all (select v from work.t) = 1
