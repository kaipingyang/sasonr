# SQLINLIST: each call is explained above it and followed by its result

# IN returns 1 if the value equals any item of the list.
sas_f_sqlinlist(2, 1, 2, 3)  # 2 in (1, 2, 3) = 1

# 'b' is not in the list, so the result is 0.
sas_f_sqlinlist("b", "a", "c")  # 'b' in ('a', 'c') = 0

# In PROC SQL a missing value equals another missing value, so . IN (., 1) is
# true.
sas_f_sqlinlist(NA, NA, 1)  # . in (., 1) = 1
