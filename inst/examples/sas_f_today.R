# TODAY: each call is explained above it and followed by its result
# (SAS dates are day counts from 1960-01-01; datetimes and times are seconds)
# TODAY() returns the current date (the clock of the session), so its value
# changes over time; the examples check its type and range only.

# TODAY() returns the current date as a SAS date value; DATE() is an alias.
as.numeric(sas_f_today() == sas_f_date())  # today() = date() = 1

# The current date is later than 1 January 2024.
as.numeric(sas_f_today() > 23376)  # today() > '01JAN2024'd = 1

# WEEKDAY of any date lies between 1 and 7.
as.numeric(sas_f_weekday(sas_f_today()) <= 7)  # weekday(today()) <= 7 = 1
