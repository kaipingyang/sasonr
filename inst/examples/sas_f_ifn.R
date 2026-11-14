# IFN: each call is explained above it and followed by its result

# A true (nonzero, nonmissing) condition returns the second argument.
sas_f_ifn(1, 10, 20)  # ifn(1, 10, 20) = 10

# A condition of 0 is false, so the third argument is returned.
sas_f_ifn(0, 10, 20)  # ifn(0, 10, 20) = 20

# A missing condition returns the optional fourth argument.
sas_f_ifn(NA_real_, 10, 20, 99)  # ifn(., 10, 20, 99) = 99

# Any nonzero, nonmissing value, including a negative number, counts as true.
sas_f_ifn(-1, 10, 20)  # ifn(-1, 10, 20) = 10
