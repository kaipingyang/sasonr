# COALESCEC: each call is explained above it and followed by the result of the same call in SAS 9.4

# COALESCEC returns the first nonblank character argument, 'b'.
sas_f_coalescec(" ", "b", "c")  # SAS 9.4: coalescec('', 'b', 'c') = "b"

# All arguments are blank, so the result is blank.
sas_f_coalescec(" ", " ")  # SAS 9.4: coalescec('', ' ') = ""

# A leading blank does not make a string blank, so ' a' is returned unchanged.
sas_f_coalescec(" a", "b")  # SAS 9.4: coalescec(' a', 'b') = " a"
