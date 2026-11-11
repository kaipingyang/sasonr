# COLLATE: each call is followed by the result of the same call in SAS 9.4
sas_f_collate(65, 70)  # SAS: collate(65, 70) -> "ABCDEF"
sas_f_collate(48, len = 10)  # SAS: collate(48, , 10) -> "0123456789"
sas_f_collate(97, 99)  # SAS: collate(97, 99) -> "abc"
