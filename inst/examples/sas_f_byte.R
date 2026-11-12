# BYTE: each call is explained above it and followed by the result of the same call in SAS 9.4

# BYTE returns the character with code 65 in the ASCII collating sequence, 'A'.
sas_f_byte(65)  # SAS 9.4: byte(65) = "A"

# Code 97 is the lowercase letter 'a'.
sas_f_byte(97)  # SAS 9.4: byte(97) = "a"

# Code 48 is the digit character '0'.
sas_f_byte(48)  # SAS 9.4: byte(48) = "0"

# A missing code gives a blank result.
sas_f_byte(NA_real_)  # SAS 9.4: byte(.) = ""
