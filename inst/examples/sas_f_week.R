# WEEK: each call is explained above it and followed by the result of the same call in SAS 9.4
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)

# With the default descriptor 'u' weeks start on Sunday and days before the
# first Sunday are week 0: week 10.
sas_f_week(23450)  # SAS 9.4: week('15MAR2024'd) = 10

# Descriptor 'v' gives the ISO 8601 week (weeks start on Monday; week 1
# contains 4 January): week 11.
sas_f_week(23450, "v")  # SAS 9.4: week('15MAR2024'd, 'v') = 11

# Descriptor 'w' counts weeks starting on Monday, with days before the first
# Monday in week 0: week 11.
sas_f_week(23450, "w")  # SAS 9.4: week('15MAR2024'd, 'w') = 11

# Under ISO 8601 rules Sunday 1 January 2023 belongs to week 52 of the previous
# year.
sas_f_week(23011, "v")  # SAS 9.4: week('01JAN2023'd, 'v') = 52
