# Runtime helpers used by translated code

Translated R code calls these helpers to apply SAS value semantics
(missing values, fixed-length characters, PDV, formats, log NOTEs). They
are exported so that translated code can run on its own; they are not a
stable user interface.
