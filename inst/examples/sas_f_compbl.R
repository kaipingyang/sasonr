# COMPBL: each call is explained above it and followed by the result of the same call in SAS 9.4

# COMPBL replaces each run of two or more blanks with a single blank.
sas_f_compbl("a   b    c")  # SAS 9.4: compbl('a   b    c') = "a b c"

# Leading blanks are also reduced to one blank, but not removed.
sas_f_compbl("  a  b")  # SAS 9.4: compbl('  a  b') = " a b"

# A string without repeated blanks is returned unchanged.
sas_f_compbl("a")  # SAS 9.4: compbl('a') = "a"
