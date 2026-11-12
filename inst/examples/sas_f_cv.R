# CV: each call is explained above it and followed by the result of the same call in SAS 9.4

# CV is the coefficient of variation: 100 times the standard deviation divided
# by the mean.
sas_f_cv(1, 2, 3, 4)  # SAS 9.4: cv(1, 2, 3, 4) = 51.6397779494322

# Values without spread have a coefficient of variation of 0.
sas_f_cv(2, 2)  # SAS 9.4: cv(2, 2) = 0

# Missing values are ignored; the CV is computed from 1 and 3.
sas_f_cv(1, NA_real_, 3)  # SAS 9.4: cv(1, ., 3) = 70.7106781186548
