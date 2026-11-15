# The function layer called directly ------------------------------------------
#
# The exported sas_f_* functions are the SAS functions for ordinary R code.  A
# call from R outside a submission passes the same gates as the function in a
# SAS program:
#   - the feature registry (strict mode, registry.R): an approximate,
#     unsupported or unregistered function fails loud with
#     `sasonr_strict_error` (a warning of class `sasonr_strict_warning` with
#     options(sasonr.strict = FALSE));
#   - the format registry for PUT() and INPUT() (a format given as text such as
#     "8.2" is parsed first) and, as before, for PUTN/PUTC/INPUTN/INPUTC;
#   - the argument domain of the policy table (fn_policy.R) through
#     sas_fcall(): arguments outside the supported domain fail loud, invalid
#     arguments give SAS's missing result;
#   - a data error (the invalid-argument NOTE, invalid data, a mathematical
#     operation that could not be performed, `_ERROR_` set) is signalled as a
#     warning of class `sasonr_data_error` that carries the SAS NOTE text.
# Inside a submission (translated code, %SYSFUNC, PROC code) and in calls that
# one function makes to another, the functions run unwrapped: the analyzer,
# sas_fcall() and the %SYSFUNC gate check them there.
#
# This file is collated last: every exported sas_f_* function, except the
# helpers of translated PROC SQL code listed in `fn_layer_internal`, is
# replaced here by a wrapper with the same arguments that calls the
# implementation through fn_layer_call().

fn_layer_internal <- c("cvt_c2n", "cvt_n2c", "sqlcase", "sqlinlist", "sqllike", "sqlmonotonic", "sqlquant")

# Arguments that name a format (PUT) or an informat (INPUT).
fn_layer_fmt_arg <- c(put = "format", input = "informat")

fn_layer_call <- function(name, impl, call, env) {
  if (!is.null(.active$session) || isTRUE(.active$fn_layer)) {
    call[[1L]] <- impl
    return(eval(call, env))
  }
  old_rt <- .active$rt
  .active$fn_layer <- TRUE
  .active$rt <- rt <- new_rt_state()
  on.exit({
    .active$fn_layer <- NULL
    .active$rt <- old_rt
  }, add = TRUE)
  mc <- match.call(impl, call, expand.dots = TRUE, envir = env)
  args <- lapply(as.list(mc)[-1L], eval, envir = env)
  feature_check("function", name, context = "data")
  if (name %in% names(fn_layer_fmt_arg)) args$fmt <- fn_layer_fmt(args$fmt, fn_layer_fmt_arg[[name]])
  res <- tryCatch(
    if (name %in% names(fn_policy)) do.call(sas_fcall, c(list(name, impl), args)) else do.call(impl, args),
    sasonr_step_exception = function(e) {
      abort_unsupported(sprintf("%s with these arguments outside a DATA step (a step exception, which stops the step)",
                                toupper(name)))
    }
  )
  fn_layer_data_errors(rt, name)
  res
}

# A format or informat of PUT()/INPUT() given as text ("8.2", "$char10.") or as
# a parsed specification; checked against the registry unless it is a user
# format.
fn_layer_fmt <- function(fmt, kind) {
  if (is.character(fmt)) {
    if (length(fmt) != 1L || is.na(fmt) || !nzchar(trimws(fmt))) {
      abort_unsupported(sprintf("a %s given as anything but one format name", kind))
    }
    fmt <- sas_fmt_text(fmt)
  }
  if (!is.list(fmt) || is.null(fmt$name)) abort_unsupported(sprintf("a %s that is neither text nor a parsed specification", kind))
  putn_check(fmt, kind)
  fmt
}

# The data errors a direct call recorded, as warnings of class sasonr_data_error.
fn_layer_data_errors <- function(rt, name) {
  msgs <- rt$notes
  if (!length(msgs) && length(rt$math)) {
    msgs <- c(msgs, "NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.")
  }
  if (!length(msgs) && isTRUE(rt$err)) {
    msgs <- sprintf("_ERROR_ is set to 1 by this call of %s.", toupper(name))
  }
  for (m in unique(msgs)) {
    warning(structure(class = c("sasonr_data_error", "warning", "condition"),
                      list(message = m, call = NULL, name = toupper(name), note = m)))
  }
  invisible()
}

fn_layer_wrap <- function(name, impl) {
  f <- function() NULL
  formals(f) <- formals(impl)
  body(f) <- bquote(fn_layer_call(.(name), impl, sys.call(), parent.frame()))
  e <- new.env(parent = environment(impl))
  e$impl <- impl
  environment(f) <- e
  f
}

local({
  ns <- topenv()
  for (fn in ls(ns, pattern = "^sas_f_")) {
    name <- sub("^sas_f_", "", fn)
    if (name %in% fn_layer_internal) next
    assign(fn, fn_layer_wrap(name, get(fn, envir = ns)), envir = ns)
  }
})
