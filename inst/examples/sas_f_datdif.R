# DATDIF: each call is followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
sas_f_datdif(23376, 23436, "ACT/ACT")  # SAS: datdif('01JAN2024'd, '01MAR2024'd, 'ACT/ACT') -> 60
sas_f_datdif(23376, 23436, "30/360")  # SAS: datdif('01JAN2024'd, '01MAR2024'd, '30/360') -> 60
sas_f_datdif(23406, 23436, "30/360")  # SAS: datdif('31JAN2024'd, '01MAR2024'd, '30/360') -> 31
sas_f_datdif(23436, 23376, "ACT/ACT")  # SAS: datdif('01MAR2024'd, '01JAN2024'd, 'ACT/ACT') -> -60
