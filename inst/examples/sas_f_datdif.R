# DATDIF: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# With basis 'ACT/ACT' DATDIF counts actual days: 31 in January plus 29 in
# February 2024 = 60.
sas_f_datdif(23376, 23436, "ACT/ACT")  # SAS 9.4: datdif('01JAN2024'd, '01MAR2024'd, 'ACT/ACT') = 60

# Basis '30/360' treats every month as 30 days, so two months are 60 days.
sas_f_datdif(23376, 23436, "30/360")  # SAS 9.4: datdif('01JAN2024'd, '01MAR2024'd, '30/360') = 60

# Under '30/360' the 31st counts as the 30th, so 31 January to 1 March is 31
# days.
sas_f_datdif(23406, 23436, "30/360")  # SAS 9.4: datdif('31JAN2024'd, '01MAR2024'd, '30/360') = 31

# If the end date is before the start date, the result is negative.
sas_f_datdif(23436, 23376, "ACT/ACT")  # SAS 9.4: datdif('01MAR2024'd, '01JAN2024'd, 'ACT/ACT') = -60
