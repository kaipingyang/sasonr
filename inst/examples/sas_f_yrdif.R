# YRDIF: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_yrdif(21915, 23558, "ACT/ACT")  # SAS: yrdif('01JAN2020'd, '01JUL2024'd, 'ACT/ACT') -> 4.49726775956284
sas_f_yrdif(21915, 23558, "30/360")  # SAS: yrdif('01JAN2020'd, '01JUL2024'd, '30/360') -> 4.5
sas_f_yrdif(21915, 23558, "ACT/365")  # SAS: yrdif('01JAN2020'd, '01JUL2024'd, 'ACT/365') -> 4.5013698630137
