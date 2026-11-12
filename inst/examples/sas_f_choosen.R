# CHOOSEN: each call is explained above it and followed by the result of the same call in SAS 9.4

# Index 2 selects the second value, 20.
sas_f_choosen(2, 10, 20, 30)  # SAS 9.4: choosen(2, 10, 20, 30) = 20

# A negative index counts from the end of the list: -1 selects the last value,
# 30.
sas_f_choosen(-1, 10, 20, 30)  # SAS 9.4: choosen(-1, 10, 20, 30) = 30

# Index 5 is beyond the two values given, so the result is missing (with a note
# in the log).
sas_f_choosen(5, 1, 2)  # SAS 9.4: choosen(5, 1, 2) = .
