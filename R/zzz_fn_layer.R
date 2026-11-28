# The function layer called directly ------------------------------------------
#
# The exported sas_f_* functions are the SAS functions for ordinary R code.  A
# call from R outside a submission passes the same gates as the function in a
# SAS program:
#   - the feature registry (strict mode, registry.R): an approximate,
#     unsupported or unregistered function fails loud with
#     `sasonr_strict_error` (with options(sasonr.strict = FALSE) a warning of
#     class `sasonr_strict_warning`, once per function and R session);
#   - the format registry for PUT() and INPUT() (a format given as text such as
#     "8.2" is parsed first) and, as before, for PUTN/PUTC/INPUTN/INPUTC;
#   - character arguments with bytes outside ASCII (sas_rt_chr_arg());
#   - the argument domain of the policy table (fn_policy.R) through
#     sas_rt_fcall(): arguments outside the exact domain go through the same
#     strict gate, invalid arguments give SAS's missing result;
#   - a data error (the invalid-argument NOTE, invalid data, a mathematical
#     operation that could not be performed, `_ERROR_` set) is signalled as a
#     warning of class `sasonr_data_error` that carries the NOTE text.
# Inside a submission (translated code, %SYSFUNC, PROC code) and in calls that
# one function makes to another, the functions run unwrapped: the analyzer,
# sas_rt_fcall() and the %SYSFUNC gate check them there.
#
# This file is collated last: every exported sas_f_* function, except the
# helpers of translated PROC SQL code listed in `fn_layer_internal`, is
# replaced here by a wrapper with the same arguments that calls the
# implementation (kept in the table `fn_layer_impl`) through fn_layer_call().

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
  # the call as written (match.call() names the matched arguments and expands
  # `...`), every non-empty argument evaluated once; empty arguments stay empty
  mc <- match.call(impl, call, expand.dots = TRUE, envir = env)
  given <- as.list(mc)[-1L]
  nms <- names(given) %||% rep("", length(given))
  empty <- vapply(given, function(a) identical(a, quote(expr = )), TRUE)
  vals <- vector("list", length(given))
  for (i in which(!empty)) {
    v <- eval(given[[i]], env)
    if (!is.null(v)) vals[[i]] <- v
  }
  feature_check("function", name, context = "data")
  # character arguments with bytes outside ASCII (sas_rt_chr_arg(); PUT and INPUT
  # check their values themselves)
  if (!name %in% c(chr_wide_ok_fns, "put", "input")) for (v in vals) sas_rt_chr_arg(v, name)
  if (name %in% names(fn_layer_fmt_arg)) {
    k <- match("fmt", nms)
    if (!is.na(k) && !empty[k]) vals[[k]] <- fn_layer_fmt(vals[[k]], fn_layer_fmt_arg[[name]])
  }
  exact <- mc
  for (i in which(!empty)) exact[i + 1L] <- list(vals[[i]])
  exact[[1L]] <- impl
  run <- function(...) eval(exact, env)
  res <- tryCatch(
    if (name %in% names(fn_policy)) {
      # the policy table reads the SAS arguments by position, as translated
      # code passes them: formals in order, NULL for an empty or omitted one
      # before a given one, `...` in place; the implementation gets the call
      # as written
      rlang::exec(sas_rt_fcall, name, run, !!!fn_layer_positional(impl, nms, vals, empty),
                  !!!if (".at" %in% nms) list(.at = vals[[match(".at", nms)]]))
    } else {
      eval(exact, env)
    },
    sasonr_step_exception = function(e) {
      abort_unsupported(sprintf("%s with these arguments outside a DATA step (a step exception, which stops the step)",
                                toupper(name)))
    }
  )
  fn_layer_data_errors(rt, name)
  res
}

# The SAS arguments of a call by position (see fn_layer_call()).
fn_layer_positional <- function(impl, nms, vals, empty) {
  out <- list()
  last <- 0L
  for (f in names(formals(impl))) {
    if (f == "...") {
      k <- which(!nms %in% names(formals(impl)))
      for (i in k) out[length(out) + 1L] <- list(if (empty[i]) NULL else vals[[i]])
      if (length(k)) last <- length(out)
      next
    }
    if (startsWith(f, ".")) next
    i <- match(f, nms)
    out[length(out) + 1L] <- list(if (is.na(i) || empty[i]) NULL else vals[[i]])
    if (!is.na(i) && !empty[i]) last <- length(out)
  }
  out[seq_len(last)]
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
  if (!length(msgs) && length(log_places_get(rt, "math.failed"))) {
    msgs <- c(msgs, "NOTE: Mathematical operations could not be performed. The result of the operation has been set to a missing value.")
  }
  if (!length(msgs) && isTRUE(rt$err)) {
    msgs <- sprintf("_ERROR_ is set to 1 by this call of %s.", toupper(name))
  }
  for (m in unique(msgs)) {
    rlang::warn(m, class = "sasonr_data_error", name = toupper(name), note = m)
  }
  invisible()
}

fn_layer_wrap <- function(name, impl) {
  f <- function() NULL
  formals(f) <- formals(impl)
  body(f) <- bquote(fn_layer_call(.(name), fn_layer_impl[[.(name)]], sys.call(), parent.frame()))
  environment(f) <- environment(impl)
  f
}

# the implementations, one table (each wrapper looks its implementation up)
fn_layer_impl <- new.env(parent = emptyenv())

local({
  ns <- topenv()
  for (fn in ls(ns, pattern = "^sas_f_")) {
    name <- sub("^sas_f_", "", fn)
    if (name %in% fn_layer_internal) next
    impl <- get(fn, envir = ns)
    assign(name, impl, envir = fn_layer_impl)
    assign(fn, fn_layer_wrap(name, impl), envir = ns)
  }
})
