# REPEAT: each call is followed by the result of the same call in SAS 9.4
sas_f_repeat("ab", 2)  # SAS: repeat('ab', 2) -> "ababab"
sas_f_repeat("x", 0)  # SAS: repeat('x', 0) -> "x"
sas_f_repeat("-", 4)  # SAS: repeat('-', 4) -> "-----"
