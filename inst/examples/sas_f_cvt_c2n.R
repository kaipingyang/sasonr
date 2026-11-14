# CVT_C2N: each call is explained above it and followed by its result

# Using a character value in arithmetic converts it to a number; leading and
# trailing blanks are ignored.
sas_f_cvt_c2n(" 12 ")  # ' 12 ' + 0 = 12

# Scientific notation such as '1e3' is read as 1000.
sas_f_cvt_c2n("1e3")  # '1e3' + 0 = 1000

# A leading minus sign and an omitted leading zero are accepted: '-.5' is -0.5.
sas_f_cvt_c2n("-.5")  # '-.5' + 0 = -0.5

# 'abc' is not a valid number, so the conversion gives a missing value (with a
# note in the log).
sas_f_cvt_c2n("abc")  # 'abc' + 0 = .
