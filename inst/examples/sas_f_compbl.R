# COMPBL: each call is explained above it and followed by its result

# COMPBL replaces each run of two or more blanks with a single blank.
sas_f_compbl("a   b    c")  # compbl('a   b    c') = "a b c"

# Leading blanks are also reduced to one blank, but not removed.
sas_f_compbl("  a  b")  # compbl('  a  b') = " a b"

# A string without repeated blanks is returned unchanged.
sas_f_compbl("a")  # compbl('a') = "a"
