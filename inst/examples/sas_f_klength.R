# KLENGTH: each call is explained above it and followed by its result

# KLENGTH counts characters without trailing blanks: 3.
sas_f_klength("abc  ")  # klength('abc  ') = 3

# A character outside ASCII counts once, while LENGTH counts its bytes
# (in a UTF-8 session).
if (isTRUE(l10n_info()[["UTF-8"]])) {
  x <- "\u00e9t\u00e9"
  print(sas_f_klength(x))  # 3
  print(sas_f_length(x))   # 5
}
