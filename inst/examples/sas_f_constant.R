# CONSTANT: each call is explained above it and followed by the result of the same call in SAS 9.4

# CONSTANT('PI') returns the mathematical constant pi.
sas_f_constant("PI")  # SAS 9.4: constant('PI') = 3.14159265358979

# 'E' returns the base of the natural logarithm.
sas_f_constant("E")  # SAS 9.4: constant('E') = 2.71828182845905

# 'EULER' returns Euler's constant, about 0.5772.
sas_f_constant("EULER")  # SAS 9.4: constant('EULER') = 0.577215664901533

# 'MACEPS' returns the machine precision of double-precision numbers, 2 to the
# power -52.
sas_f_constant("MACEPS")  # SAS 9.4: constant('MACEPS') = 2.22044604925031e-16

# 'BIG' returns the largest double-precision number.
sas_f_constant("BIG")  # SAS 9.4: constant('BIG') = 1.79769313486232e+308
