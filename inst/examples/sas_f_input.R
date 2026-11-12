# INPUT: each call is explained above it and followed by the result of the same call in SAS 9.4

# INPUT reads the character value '123' with the numeric informat 8. and
# returns the number 123.
sas_f_input("123", list(name = "", w = 8L, d = NA_integer_, char = FALSE, text = "8."))  # SAS 9.4: input('123', 8.) = 123

# With informat 4.2 and no decimal point in the data, the last 2 digits are
# taken as decimals: 12.34.
sas_f_input("1234", list(name = "", w = 4L, d = 2L, char = FALSE, text = "4.2"))  # SAS 9.4: input('1234', 4.2) = 12.34

# The COMMA informat removes the commas before reading the number.
sas_f_input("1,234", list(name = "comma", w = 8L, d = NA_integer_, char = FALSE, text = "comma8."))  # SAS 9.4: input('1,234', comma8.) = 1234

# The ?? modifier suppresses the invalid-data note and the error flag; the
# unreadable value gives missing.
sas_f_input("abc", list(name = "", w = 8L, d = NA_integer_, char = FALSE, text = "8."), qq = "??")  # SAS 9.4: input('abc', ?? 8.) = .

# The character informat $3. reads only the first 3 characters.
sas_f_input("abcdef", list(name = "", w = 3L, d = NA_integer_, char = TRUE, text = "$3."))  # SAS 9.4: input('abcdef', $3.) = "abc"
