# SQLQUANT: each call is explained above it and followed by its result

# ANY is true if the comparison holds for at least one value of the subquery: 1
# < 2 holds.
sas_f_sqlquant(1, c(2, 3), "<", "any")  # 1 < any (select v from work.t) = 1

# ALL requires the comparison to hold for every value; 3 > 3 is false, so the
# result is 0.
sas_f_sqlquant(3, c(2, 3), ">", "all")  # 3 > all (select v from work.t) = 0

# 3 >= 2 and 3 >= 3 both hold, so the result is 1.
sas_f_sqlquant(3, c(2, 3), ">=", "all")  # 3 >= all (select v from work.t) = 1
