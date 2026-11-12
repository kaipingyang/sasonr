# STD: each call is explained above it and followed by the result of the same call in SAS 9.4

# STD returns the sample standard deviation (divisor n - 1).
sas_f_std(1, 2, 3, 4)  # SAS 9.4: std(1, 2, 3, 4) = 1.29099444873581

# Missing values are ignored: the standard deviation of 5 and 7 is sqrt(2).
sas_f_std(5, NA_real_, 7)  # SAS 9.4: std(5, ., 7) = 1.4142135623731

# At least two nonmissing values are needed, so the result is missing.
sas_f_std(5, NA_real_)  # SAS 9.4: std(5, .) = .

# The sample standard deviation of eight values.
sas_f_std(2, 4, 4, 4, 5, 5, 7, 9)  # SAS 9.4: std(2, 4, 4, 4, 5, 5, 7, 9) = 2.1380899352994
