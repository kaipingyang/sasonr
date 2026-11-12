# SQLINLIST: each call is explained above it and followed by the result of the same call in SAS 9.4

# IN returns 1 if the value equals any item of the list.
sas_f_sqlinlist(2, 1, 2, 3)  # SAS 9.4: 2 in (1, 2, 3) = 1

# 'b' is not in the list, so the result is 0.
sas_f_sqlinlist("b", "a", "c")  # SAS 9.4: 'b' in ('a', 'c') = 0

# In PROC SQL a missing value equals another missing value, so . IN (., 1) is
# true.
sas_f_sqlinlist(NA, NA, 1)  # SAS 9.4: . in (., 1) = 1
