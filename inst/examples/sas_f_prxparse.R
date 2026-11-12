# PRXPARSE: each call is explained above it and followed by the result of the same call in SAS 9.4

# PRXPARSE compiles a Perl regular expression and returns a numeric pattern
# identifier; the first one is 1.
sas_f_prxparse("/a+/")  # SAS 9.4: prxparse('/a+/') = 1

# Each further PRXPARSE call gets the next identifier, 2.
sas_f_prxparse("/b/i")  # SAS 9.4: prxparse('/b/i') = 2
