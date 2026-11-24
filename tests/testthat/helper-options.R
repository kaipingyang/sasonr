# The test options are set in setup-options.R (run by the test runner only,
# not by pkgload::load_all(), so they do not leak into a development session).

# A direct call of an approximate or unsupported function with
# sasonr.strict = FALSE, its sasonr_strict_warning muffled (the strict gate of
# direct calls is tested in test-fn-layer.R).
lenient <- function(expr) {
  withr::with_options(list(sasonr.strict = FALSE), withCallingHandlers(
    expr, sasonr_strict_warning = function(w) invokeRestart("muffleWarning")))
}

# The messages and PUT output of a result (its log without the source echo).
msgs <- function(r) as.character(sas_log(r, echo = FALSE))
