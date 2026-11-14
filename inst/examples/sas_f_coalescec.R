# COALESCEC: each call is explained above it and followed by its result

# COALESCEC returns the first nonblank character argument, 'b'.
sas_f_coalescec(" ", "b", "c")  # coalescec('', 'b', 'c') = "b"

# All arguments are blank, so the result is blank.
sas_f_coalescec(" ", " ")  # coalescec('', ' ') = ""

# A leading blank does not make a string blank, so ' a' is returned unchanged.
sas_f_coalescec(" a", "b")  # coalescec(' a', 'b') = " a"
