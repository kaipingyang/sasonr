# Most tests compare PUT output only; step-level NOTEs are tested in test-notes.R.
options(sasonr.step_notes = FALSE)
# Unit tests that expect a SAS ERROR check the classed condition
# (sas_run(on_error = "abort")); the SAS batch behaviour (on_error = "sas", the
# default) is tested in test-on-error.R.
options(sasonr.on_error = "abort")
