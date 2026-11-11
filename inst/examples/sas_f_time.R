# TIME: each call is followed by the result of the same call in SAS 9.4
# TIME() returns the current time of day (the clock of the session), so its
# value cannot be reproduced; the examples check its type and range only.
as.numeric(sas_f_time() >= 0)  # SAS: time() >= 0 -> 1
as.numeric(sas_f_time() < 86400)  # SAS: time() < 86400 -> 1
