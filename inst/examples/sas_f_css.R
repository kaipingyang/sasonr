# CSS: each call is explained above it and followed by the result of the same call in SAS 9.4

# CSS is the corrected sum of squares about the mean (2.5): 2.25 + 0.25 + 0.25
# + 2.25 = 5.
sas_f_css(1, 2, 3, 4)  # SAS 9.4: css(1, 2, 3, 4) = 5

# Missing values are ignored; the mean of 1 and 3 is 2, so CSS = 1 + 1 = 2.
sas_f_css(1, NA_real_, 3)  # SAS 9.4: css(1, ., 3) = 2

# A single value equals its own mean, so CSS is 0.
sas_f_css(5)  # SAS 9.4: css(5) = 0

# All arguments are missing, so the result is missing.
sas_f_css(NA_real_, NA_real_)  # SAS 9.4: css(., .) = .
