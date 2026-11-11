# INPUT: each call with the result of the same INPUT call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_inputn("123", "8.")  # SAS input('123',8.) = 123
sas_f_inputn("1234", "4.2")  # SAS input('1234',4.2) = 12.34
sas_f_inputn("12.34", "5.1")  # SAS input('12.34',5.1) = 12.34
sas_f_inputn("1,234", "comma8.")  # SAS input('1,234',comma8.) = 1234
sas_f_inputn("(12)", "comma5.")  # SAS input('(12)',comma5.) = -12
