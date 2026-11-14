# BYTE: each call is explained above it and followed by its result

# BYTE returns the character with code 65 in the ASCII collating sequence, 'A'.
sas_f_byte(65)  # byte(65) = "A"

# Code 97 is the lowercase letter 'a'.
sas_f_byte(97)  # byte(97) = "a"

# Code 48 is the digit character '0'.
sas_f_byte(48)  # byte(48) = "0"

# A missing code gives a blank result.
sas_f_byte(NA_real_)  # byte(.) = ""
