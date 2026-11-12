# INPUT: each call is explained above it and followed by the result of the same call in SAS 9.4
# (dates are SAS day counts from 1960-01-01)

# Read the text '123' with the 8. informat.
sas_f_inputn("123", "8.")  # SAS 9.4: input('123',8.) = 123

# Without a decimal point in the text, the informat 4.2 places two implied
# decimals: 12.34.
sas_f_inputn("1234", "4.2")  # SAS 9.4: input('1234',4.2) = 12.34

# A decimal point in the text overrides the informat's decimals.
sas_f_inputn("12.34", "5.1")  # SAS 9.4: input('12.34',5.1) = 12.34

# COMMA8. ignores the thousands separator.
sas_f_inputn("1,234", "comma8.")  # SAS 9.4: input('1,234',comma8.) = 1234

# COMMA5. reads a value in parentheses as negative.
sas_f_inputn("(12)", "comma5.")  # SAS 9.4: input('(12)',comma5.) = -12
