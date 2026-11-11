# BYTE: each call is followed by the result of the same call in SAS 9.4
sas_f_byte(65)  # SAS: byte(65) -> "A"
sas_f_byte(97)  # SAS: byte(97) -> "a"
sas_f_byte(48)  # SAS: byte(48) -> "0"
sas_f_byte(NA_real_)  # SAS: byte(.) -> ""
