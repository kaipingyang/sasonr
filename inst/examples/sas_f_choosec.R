# CHOOSEC: each call is explained above it and followed by its result

# Index 2 selects the second value, 'b'.
sas_f_choosec(2, "a", "b", "c")  # choosec(2, 'a', 'b', 'c') = "b"

# A negative index counts from the end of the list: -1 selects the last value,
# 'c'.
sas_f_choosec(-1, "a", "b", "c")  # choosec(-1, 'a', 'b', 'c') = "c"

# Index 4 is beyond the two values given, so the result is blank (with a note
# in the log).
sas_f_choosec(4, "a", "b")  # choosec(4, 'a', 'b') = ""
