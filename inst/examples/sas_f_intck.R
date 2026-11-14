# INTCK: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Discrete method (default): one month boundary between 31 Jan and 1 Feb 2020.
sas_f_intck("month", 21945, 21946)  # intck('month','31JAN2020'd,'01FEB2020'd) = 1

# Continuous method 'c': less than a full month between 31 Jan and 1 Feb 2020,
# so 0.
sas_f_intck("month", 21945, 21946, "c")  # intck('month','31JAN2020'd,'01FEB2020'd,'c') = 0

# One year boundary between 31 Dec 2019 and 1 Jan 2020.
sas_f_intck("year", 21914, 21915)  # intck('year','31DEC2019'd,'01JAN2020'd) = 1

# A start after the end counts boundaries backwards: -2 months.
sas_f_intck("month", 21975, 21915)  # intck('month','01MAR2020'd,'01JAN2020'd) = -2
