# TIME: each call is explained above it and followed by its result
# TIME() returns the current time of day (the clock of the session), so its
# value changes over time; the examples check its type and range only.

# TIME() returns the current time of day in seconds since midnight, which is
# never negative.
as.numeric(sas_f_time() >= 0)  # time() >= 0 = 1

# The time of day is always less than 86400 seconds (24 hours).
as.numeric(sas_f_time() < 86400)  # time() < 86400 = 1
