# COALESCE: each call is explained above it and followed by the result of the same call in SAS 9.4

# COALESCE returns the first nonmissing argument, 2.
sas_f_coalesce(NA_real_, 2, 3)  # SAS 9.4: coalesce(., 2, 3) = 2

# All arguments are missing, so the result is missing.
sas_f_coalesce(NA_real_, NA_real_)  # SAS 9.4: coalesce(., .) = .

# Special missing values (.A, ._) are skipped like ., so the first nonmissing
# value is 5.
sas_f_coalesce(sas_missing(".A"), sas_missing("._"), 5)  # SAS 9.4: coalesce(.A, ._, 5) = 5
