# INTNX: each call is explained above it and followed by the result of the same call in SAS 9.4
# (dates are SAS day counts from 1960-01-01)

# Advance 1 month: the beginning of the next month (alignment 'b' is the
# default).
sas_f_intnx("month", 21929, 1)  # SAS 9.4: intnx('month','15JAN2020'd,1) = 21946

# Alignment 'e': the end of the next month.
sas_f_intnx("month", 21929, 1, "e")  # SAS 9.4: intnx('month','15JAN2020'd,1,'e') = 21974

# Alignment 's': the same day in the next month.
sas_f_intnx("month", 21929, 1, "s")  # SAS 9.4: intnx('month','15JAN2020'd,1,'s') = 21960

# Alignment 's' from 31 Jan: the same day does not exist in February, so the
# last day (29 Feb 2020).
sas_f_intnx("month", 21945, 1, "s")  # SAS 9.4: intnx('month','31JAN2020'd,1,'s') = 21974

# A negative increment goes back: the beginning of the previous month.
sas_f_intnx("month", 21929, -1)  # SAS 9.4: intnx('month','15JAN2020'd,-1) = 21884
