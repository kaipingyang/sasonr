# COLLATE: each call is explained above it and followed by the result of the same call in SAS 9.4

# COLLATE returns the characters with codes 65 through 70 of the collating
# sequence, 'ABCDEF'.
sas_f_collate(65, 70)  # SAS 9.4: collate(65, 70) = "ABCDEF"

# With the end position omitted, the third argument is the length: 10
# characters starting at code 48 ('0').
sas_f_collate(48, len = 10)  # SAS 9.4: collate(48, , 10) = "0123456789"

# Codes 97 through 99 are 'abc'.
sas_f_collate(97, 99)  # SAS 9.4: collate(97, 99) = "abc"
