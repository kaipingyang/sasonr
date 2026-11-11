# INPUT: each call is followed by the result of the same call in SAS 9.4
sas_f_input("123", list(name = "", w = 8L, d = NA_integer_, char = FALSE, text = "8."))  # SAS: input('123', 8.) -> 123
sas_f_input("1234", list(name = "", w = 4L, d = 2L, char = FALSE, text = "4.2"))  # SAS: input('1234', 4.2) -> 12.34
sas_f_input("1,234", list(name = "comma", w = 8L, d = NA_integer_, char = FALSE, text = "comma8."))  # SAS: input('1,234', comma8.) -> 1234
sas_f_input("abc", list(name = "", w = 8L, d = NA_integer_, char = FALSE, text = "8."), qq = "??")  # SAS: input('abc', ?? 8.) -> .
sas_f_input("abcdef", list(name = "", w = 3L, d = NA_integer_, char = TRUE, text = "$3."))  # SAS: input('abcdef', $3.) -> "abc"
