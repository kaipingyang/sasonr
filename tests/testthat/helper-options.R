# Most tests compare PUT output only; step-level NOTEs are tested in test-notes.R.
options(sasonr.step_notes = FALSE)
# Unit tests that expect a SAS ERROR check the classed condition
# (sas_run(on_error = "abort")); the batch-mode error handling (on_error =
# "sas", the default) is tested in test-on-error.R.
options(sasonr.on_error = "abort")

# A direct call of an approximate or unsupported function with
# sasonr.strict = FALSE, its sasonr_strict_warning muffled (the strict gate of
# direct calls is tested in test-fn-layer.R).
lenient <- function(expr) {
  withr::with_options(list(sasonr.strict = FALSE), withCallingHandlers(
    expr, sasonr_strict_warning = function(w) invokeRestart("muffleWarning")))
}
