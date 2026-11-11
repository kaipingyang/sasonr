# CSS: each call is followed by the result of the same call in SAS 9.4
sas_f_css(1, 2, 3, 4)  # SAS: css(1, 2, 3, 4) -> 5
sas_f_css(1, NA_real_, 3)  # SAS: css(1, ., 3) -> 2
sas_f_css(5)  # SAS: css(5) -> 0
sas_f_css(NA_real_, NA_real_)  # SAS: css(., .) -> .
