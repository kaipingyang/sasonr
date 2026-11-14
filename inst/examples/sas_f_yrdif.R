# YRDIF: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# '30/360' counts 30-day months and 360-day years: 54 months are 4.5 years.
sas_f_yrdif(21915, 23558, "30/360")  # yrdif('01JAN2020'd, '01JUL2024'd, '30/360') = 4.5

# 'ACT/365' divides the actual number of days by 365.
sas_f_yrdif(21915, 23558, "ACT/365")  # yrdif('01JAN2020'd, '01JUL2024'd, 'ACT/365') = 4.5013698630137
