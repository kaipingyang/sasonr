# PUT: each call is explained above it and followed by the result of the same call in SAS 9.4

# PUT formats 3.14159 with the w.d format 8.3: rounded to 3 decimals,
# right-aligned in 8 columns.
sas_f_put(3.14159, list(name = "", w = 8L, d = 3L, char = FALSE, text = "8.3"))  # SAS 9.4: put(3.14159, 8.3) = "   3.142"

# The Z5. format pads the number with leading zeros to width 5.
sas_f_put(42, list(name = "z", w = 5L, d = NA_integer_, char = FALSE, text = "z5."))  # SAS 9.4: put(42, z5.) = "00042"

# The COMMA10. format inserts thousands separators, right-aligned in 10
# columns.
sas_f_put(1234567, list(name = "comma", w = 10L, d = NA_integer_, char = FALSE, text = "comma10."))  # SAS 9.4: put(1234567, comma10.) = " 1,234,567"

# A missing value is written as a period; for a w.d format it sits where the
# decimal point would be.
sas_f_put(NA_real_, list(name = "", w = 8L, d = 2L, char = FALSE, text = "8.2"))  # SAS 9.4: put(., 8.2) = "     ."

# The character format $5. writes 'ab' in a 5-character field, padded with
# trailing blanks.
sas_f_put("ab", list(name = "", w = 5L, d = NA_integer_, char = TRUE, text = "$5."))  # SAS 9.4: put('ab', $5.) = "ab"
