# Most tests compare PUT output only; step-level NOTEs are tested in test-notes.R.
options(sasonr.step_notes = FALSE)
# Unit tests that expect a SAS ERROR check the classed condition
#
# default) is tested in test-on-error.R.
options(sasonr.on_error = "abort")
