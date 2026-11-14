# IFC: each call is explained above it and followed by its result

# A true (nonzero, nonmissing) condition returns the second argument.
sas_f_ifc(1, "yes", "no")  # ifc(1, 'yes', 'no') = "yes"

# A condition of 0 is false, so the third argument is returned.
sas_f_ifc(0, "yes", "no")  # ifc(0, 'yes', 'no') = "no"

# A missing condition returns the optional fourth argument.
sas_f_ifc(NA_real_, "yes", "no", "miss")  # ifc(., 'yes', 'no', 'miss') = "miss"

# A comparison evaluates to 1 (true) or 0 (false); 2 > 1 is true.
sas_f_ifc(2 > 1, "a", "b")  # ifc(2 > 1, 'a', 'b') = "a"
