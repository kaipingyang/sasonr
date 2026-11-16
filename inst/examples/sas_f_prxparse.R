# PRXPARSE: each call is explained above it and followed by its result

# Outside sas_run() the PRXPARSE ids are kept in an explicit session.
s <- sas_session()

# PRXPARSE compiles a Perl regular expression and returns a numeric pattern
# identifier; the first one is 1.
sas_f_prxparse("/a+/", session = s)  # prxparse('/a+/') = 1

# Each further PRXPARSE call gets the next identifier, 2.
sas_f_prxparse("/b/i", session = s)  # prxparse('/b/i') = 2
