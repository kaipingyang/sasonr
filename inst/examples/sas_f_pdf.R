# PDF: each call is explained above it and followed by its result

# PDF is approximate: in strict mode (the default) a direct call stops; these calls
# run with options(sasonr.strict = FALSE) and a warning of class sasonr_strict_warning.
old <- options(sasonr.strict = FALSE)

# The standard uniform density is 1 everywhere inside from 0 to 1.
sas_f_pdf("UNIFORM", 0.5)  # pdf('UNIFORM', 0.5) = 1

# A missing quantile gives a missing result.
sas_f_pdf("NORMAL", NA_real_)  # pdf('NORMAL', .) = .

options(old)
