# PUT: each call with the result of the same PUT call in SAS 9.4
# (offline conformance cases; dates are SAS day counts from 1960-01-01)
sas_f_putn(123, "8.")  # SAS put(123,8.) = "     123"
sas_f_putn(12.345, "8.2")  # SAS put(12.345,8.2) = "   12.35"
sas_f_putn(2.5, "1.")  # SAS put(2.5,1.) = "3"
sas_f_putn(1e10, "8.")  # SAS put(1e10,8.) = "    1E10"
sas_f_putn(-1234.5, "dollar10.2")  # SAS put(-1234.5,dollar10.2) = "$-1,234.50"
sas_f_putn(0.125, "percent8.1")  # SAS put(0.125,percent8.1) = "  12.5%"
