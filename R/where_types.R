# WHERE expressions do not convert function arguments between character and
# numeric (SAS 9.4, fn_where matrix): a character value where the function
# expects a number is "ERROR: Function ABS requires a numeric expression as
# argument 1." (and the converse with "character expression"); the step stops.
# Declared argument types come from the SAS 9.4 function inventory (feature
# registry column sas_arg_types; the last type repeats).
where_arg_types <- function(e, st) {
  reg <- sas_feature_registry("functions")
  ty <- reg$sas_arg_types[toupper(reg$name) == toupper(e$name) & reg$kind == "function"]
  if (!length(ty) || is.na(ty[1]) || !nzchar(ty[1])) return(invisible())
  decl <- strsplit(ty[1], " ", fixed = TRUE)[[1]]
  for (k in seq_along(e$args)) {
    a <- e$args[[k]]
    if (!is.list(a) || is.null(a$node) || a$node %in% c("of", "omitted")) next # omitted arguments (COMPRESS, SCAN)
    want <- decl[min(k, length(decl))]
    if (!want %in% c("num", "char")) next
    got <- expr_type(a, st)$type
    if (identical(want, "num") && identical(got, "char")) {
      abort_syntax(sprintf("Function %s requires a numeric expression as argument %d.", toupper(e$name), k), e$line)
    }
    if (identical(want, "char") && identical(got, "num")) {
      abort_syntax(sprintf("Function %s requires a character expression as argument %d.", toupper(e$name), k), e$line)
    }
  }
  invisible()
}
