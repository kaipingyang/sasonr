# ROUND: each call is explained above it and followed by its result
# (dates are SAS day counts from 1960-01-01)

# Round to the nearest integer: halves go away from zero.
sas_f_round(2.5)  # round(2.5) = 3

# Negative halves also go away from zero: -3.
sas_f_round(-2.5)  # round(-2.5) = -3

# Rounding unit 0.1: ROUND treats 2.45 as the exact decimal, so 2.5 (base R
# gives 2.4).
sas_f_round(2.45, 0.1)  # round(2.45,0.1) = 2.5

# Rounding unit 0.01: 1.005 gives 1.01.
sas_f_round(1.005, 0.01)  # round(1.005,0.01) = 1.01

# Rounding unit 100: the nearest multiple of 100.
sas_f_round(1234.5678, 100)  # round(1234.5678,100) = 1200

# A rounding unit of 1/3: the nearest multiple of 1/3.
sas_f_round(2/3, 1/3)  # round(2/3,1/3) = 0.66666666666666
