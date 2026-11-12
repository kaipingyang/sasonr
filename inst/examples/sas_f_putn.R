# PUT: each call is explained above it and followed by the result of the same call in SAS 9.4
# (dates are SAS day counts from 1960-01-01)

# Write 123 with the 8. format: right-aligned in 8 columns.
sas_f_putn(123, "8.")  # SAS 9.4: put(123,8.) = "     123"

# 8.2 rounds to two decimals.
sas_f_putn(12.345, "8.2")  # SAS 9.4: put(12.345,8.2) = "   12.35"

# Format 1. rounds 2.5 half away from zero: 3.
sas_f_putn(2.5, "1.")  # SAS 9.4: put(2.5,1.) = "3"

# A value too wide for 8. is written in E notation.
sas_f_putn(1e10, "8.")  # SAS 9.4: put(1e10,8.) = "    1E10"

# DOLLAR10.2: dollar sign, thousands separator, minus sign.
sas_f_putn(-1234.5, "dollar10.2")  # SAS 9.4: put(-1234.5,dollar10.2) = "$-1,234.50"

# PERCENT8.1 multiplies by 100 and adds a percent sign.
sas_f_putn(0.125, "percent8.1")  # SAS 9.4: put(0.125,percent8.1) = "  12.5%"
