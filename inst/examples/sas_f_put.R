# PUT: each call is followed by the result of the same call in SAS 9.4
sas_f_put(3.14159, list(name = "", w = 8L, d = 3L, char = FALSE, text = "8.3"))  # SAS: put(3.14159, 8.3) -> "   3.142"
sas_f_put(42, list(name = "z", w = 5L, d = NA_integer_, char = FALSE, text = "z5."))  # SAS: put(42, z5.) -> "00042"
sas_f_put(1234567, list(name = "comma", w = 10L, d = NA_integer_, char = FALSE, text = "comma10."))  # SAS: put(1234567, comma10.) -> " 1,234,567"
sas_f_put(NA_real_, list(name = "", w = 8L, d = 2L, char = FALSE, text = "8.2"))  # SAS: put(., 8.2) -> "     ."
sas_f_put("ab", list(name = "", w = 5L, d = NA_integer_, char = TRUE, text = "$5."))  # SAS: put('ab', $5.) -> "ab"
