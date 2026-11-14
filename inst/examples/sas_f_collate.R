# COLLATE: each call is explained above it and followed by its result

# COLLATE returns the characters with codes 65 through 70 of the collating
# sequence, 'ABCDEF'.
sas_f_collate(65, 70)  # collate(65, 70) = "ABCDEF"

# With the end position omitted, the third argument is the length: 10
# characters starting at code 48 ('0').
sas_f_collate(48, len = 10)  # collate(48, , 10) = "0123456789"

# Codes 97 through 99 are 'abc'.
sas_f_collate(97, 99)  # collate(97, 99) = "abc"
