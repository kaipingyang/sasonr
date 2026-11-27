# Options of the test run only: setup files run before the tests and are not
# sourced by pkgload::load_all(), unlike helper files, so a development
# session keeps the package defaults (step NOTEs on, on_error = "sas").
# Most tests compare PUT output only; step-level NOTEs are tested in test-notes.R.
# Unit tests that expect an ERROR of the program check the classed condition
# (sas_run(on_error = "abort")); the batch-mode error handling (on_error =
# "sas", the default) is tested in test-on-error.R.
withr::local_options(sasonr.step_notes = FALSE, sasonr.on_error = "abort", .local_envir = testthat::teardown_env())
# Listing tests compare the NOCENTER layout; the CENTER layout (the default of
# a session) is tested in test-listing-center.R.
withr::local_options(sasonr.center = FALSE, .local_envir = testthat::teardown_env())
# Every program the tests parse is checked for well-formed AST nodes
# (validate_sas_node(), spec 09 L3d).
withr::local_options(sasonr.validate_ast = TRUE, .local_envir = testthat::teardown_env())
