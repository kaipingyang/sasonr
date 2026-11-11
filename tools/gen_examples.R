# Generate inst/examples/sas_f_<fn>.R from SAS-verified offline conformance
# cases (tests/conformance/functions/<FN>/<FN>_<id>/).  Every example line is
# an R call followed by the result real SAS 9.4 gave for the same call
# (expected/files/results.txt of the case).
#
# Usage (package root; the local conformance assets must be present):
#   Rscript tools/gen_examples.R
# The calls are listed here (case id -> R call); the SAS results come from the
# gold standard, so an example is only written for a case whose gold exists.

calls <- list(
  CATX = c(`001` = 'sas_f_catx("-", "a", "b", "c")', `002` = 'sas_f_catx("-", "  a ", "b  ", " c")',
           `003` = 'sas_f_catx("-", "a", " ", "c")', `006` = 'sas_f_catx("-", "a", NA, "c")'),
  COMPRESS = c(`001` = 'sas_f_compress("a b c")', `002` = 'sas_f_compress("a-b-c", "-")',
               `004` = 'sas_f_compress("a1 b2", "", "d")', `006` = 'sas_f_compress("a1 b2", "", "kd")',
               `007` = 'sas_f_compress("ABC abc", "a", "i")'),
  INDEX = c(`001` = 'sas_f_index("abcabc", "bc")', `002` = 'sas_f_index("abc", "d")',
            `004` = 'sas_f_index("ABC", "b")', `005` = 'sas_f_index("a bc", " ")'),
  INPUT = c(`001` = 'sas_f_inputn("123", "8.")', `004` = 'sas_f_inputn("1234", "4.2")',
            `005` = 'sas_f_inputn("12.34", "5.1")', `006` = 'sas_f_inputn("1,234", "comma8.")',
            `008` = 'sas_f_inputn("(12)", "comma5.")'),
  INTCK = c(`001` = 'sas_f_intck("month", 21945, 21946)', `002` = 'sas_f_intck("month", 21945, 21946, "c")',
            `006` = 'sas_f_intck("year", 21914, 21915)', `009` = 'sas_f_intck("month", 21975, 21915)'),
  INTNX = c(`001` = 'sas_f_intnx("month", 21929, 1)', `002` = 'sas_f_intnx("month", 21929, 1, "e")',
            `004` = 'sas_f_intnx("month", 21929, 1, "s")', `005` = 'sas_f_intnx("month", 21945, 1, "s")',
            `008` = 'sas_f_intnx("month", 21929, -1)'),
  PUT = c(`001` = 'sas_f_putn(123, "8.")', `003` = 'sas_f_putn(12.345, "8.2")', `005` = 'sas_f_putn(2.5, "1.")',
          `008` = 'sas_f_putn(1e10, "8.")', `011` = 'sas_f_putn(-1234.5, "dollar10.2")',
          `012` = 'sas_f_putn(0.125, "percent8.1")'),
  ROUND = c(`001` = 'sas_f_round(2.5)', `002` = 'sas_f_round(-2.5)', `005` = 'sas_f_round(2.45, 0.1)',
            `006` = 'sas_f_round(1.005, 0.01)', `007` = 'sas_f_round(1234.5678, 100)', `011` = 'sas_f_round(2/3, 1/3)'),
  SCAN = c(`001` = 'sas_f_scan("a b c", 1)', `004` = 'sas_f_scan("a b c", -1)', `003` = 'sas_f_scan("a b c", 4)',
           `011` = 'sas_f_scan("a,b,,c", 3)'),
  SUBSTR = c(`001` = 'sas_f_substr("abcdef", 2)', `002` = 'sas_f_substr("abcdef", 2, 3)',
             `006` = 'sas_f_substr("abcdef", 7)', `011` = 'sas_f_substr("abcdef", 2, 10)')
)

root <- file.path("tests", "conformance", "functions")
if (!dir.exists(root)) stop("tests/conformance/functions not found (local conformance assets)")
dir.create(file.path("inst", "examples"), recursive = TRUE, showWarnings = FALSE)

sas_result <- function(fn, id) {
  case <- file.path(root, fn, paste0(fn, "_", id))
  meta <- readLines(file.path(case, "meta.yaml"))
  expr <- sub('^.*\\[(.*)\\]"?$', "\\1", grep("^title:", meta, value = TRUE))
  f <- file.path(case, "expected", "files", "results.txt")
  if (!file.exists(f)) return(NULL)
  r <- readLines(f, warn = FALSE)
  q <- grep(paste0("^@@Q ", fn, "_", id, " "), r, value = TRUE)
  n <- grep(paste0("^@@N ", fn, "_", id, " "), r, value = TRUE)
  val <- if (length(q)) sub(paste0("^@@Q ", fn, "_", id, " "), "", q[1]) else
    if (length(n)) trimws(sub("^.*best=", "", n[1])) else return(NULL)
  list(expr = gsub("\\\\'", "'", expr), value = val)
}

for (fn in names(calls)) {
  out <- c(sprintf("# %s: each call with the result of the same %s call in SAS 9.4", fn, fn),
           "# (offline conformance cases; dates are SAS day counts from 1960-01-01)")
  for (id in names(calls[[fn]])) {
    g <- sas_result(fn, id)
    if (is.null(g)) next
    out <- c(out, sprintf("%s  # SAS %s = %s", calls[[fn]][[id]], g$expr, g$value))
  }
  writeLines(out, file.path("inst", "examples", paste0("sas_f_", tolower(if (fn == "INPUT") "inputn" else if (fn == "PUT") "putn" else fn), ".R")))
}
cat("examples written for", length(calls), "functions\n")
