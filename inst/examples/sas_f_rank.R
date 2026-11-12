# RANK: each call is explained above it and followed by the result of the same call in SAS 9.4

# RANK returns the position of a character in the ASCII collating sequence: 'A'
# is 65.
sas_f_rank("A")  # SAS 9.4: rank('A') = 65

# The lowercase letter 'a' is 97.
sas_f_rank("a")  # SAS 9.4: rank('a') = 97

# The blank is 32.
sas_f_rank(" ")  # SAS 9.4: rank(' ') = 32

# The digit character '0' is 48.
sas_f_rank("0")  # SAS 9.4: rank('0') = 48
