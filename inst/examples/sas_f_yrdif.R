# YRDIF: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# 'ACT/ACT' splits the period by calendar year and divides the days in each
# year by 365 or 366. Across calendar years the sum of the fractions is outside
# the verified domain (it can differ from SAS in the last bits): sasonr fails loud.
try(sas_f_yrdif(21915, 23558, "ACT/ACT"))  # SAS 9.4: yrdif('01JAN2020'd, '01JUL2024'd, 'ACT/ACT') = 4.49726775956284

# 'AGE' computes an age in years: one day before the 24th anniversary gives
# just under 24. Fractional ages are outside the verified domain (last bits):
# sasonr fails loud; whole years are verified.
try(sas_f_yrdif(14684, 23449, "AGE"))  # SAS 9.4: yrdif('15MAR2000'd, '14MAR2024'd, 'AGE') = 23.9972602739726

# '30/360' counts 30-day months and 360-day years: 54 months are 4.5 years.
sas_f_yrdif(21915, 23558, "30/360")  # SAS 9.4: yrdif('01JAN2020'd, '01JUL2024'd, '30/360') = 4.5

# 'ACT/365' divides the actual number of days by 365.
sas_f_yrdif(21915, 23558, "ACT/365")  # SAS 9.4: yrdif('01JAN2020'd, '01JUL2024'd, 'ACT/365') = 4.5013698630137
