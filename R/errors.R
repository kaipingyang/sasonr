# SAS error model of sas_run(on_error = "sas") (spec 01 §4.7, decision D4).
#
# SAS in batch: a step that raises an ERROR is not executed (or stops); SAS
# writes "NOTE: The SAS System stopped processing this step because of
# errors." and, outside PROC SQL, enters syntax-check mode (OBS=0): the steps
# that follow are compiled but not run -- a DATA step creates its output data
# sets with 0 observations, PROC steps produce nothing -- while global
# statements and the macro language are still processed.  An ERROR inside
# PROC SQL sets NOEXEC for the rest of that PROC SQL step only.  Output data
# sets of the failing step: a new one is created with 0 observations and the
# variables compiled so far; an existing one is not replaced.  SYSERR/SYSCC
# follow SAS (SAS gold tests/conformance/datastep/errors).
#
# Only SAS ERRORs (`sasonr_syntax_error`, `sasonr_runtime_error`) are handled
# this way.  `sasonr_unsupported` (fail loud) and R errors always abort.  An
# execution-time ERROR in a DATA step (data sets partly written) still aborts
# with `sasonr_unsupported`: the partial data sets are not reproduced yet.

err_message_lines <- function(e) {
  msg <- conditionMessage(e)
  msg <- sub("^(Syntax error|Runtime error): ", "", msg)
  msg <- sub(" \\(SAS line [0-9]+\\)$", "", msg)
  code <- regmatches(msg, regexpr("\\(ERROR [0-9]+-[0-9]+\\)$", msg))
  if (length(code) == 1L) {
    msg <- trimws(sub("\\(ERROR [0-9]+-[0-9]+\\)$", "", msg))
    return(paste0("ERROR ", gsub("[^0-9-]", "", code), ": ", msg))
  }
  paste0("ERROR: ", msg)
}

# Statements of a chunk of SAS text as token tables (NULL if it cannot be lexed).
err_statements <- function(text) {
  tk <- tryCatch(sas_lex(text), error = function(e) NULL)
  if (is.null(tk) || nrow(tk) == 0) return(NULL)
  id <- cumsum(c(0L, utils::head(tk$type == "semi", -1L)))
  out <- split(tk, id)
  lapply(out, function(s) s[s$type != "semi", , drop = FALSE])
}

# A [libref.]name at token i: list(name, i = next token) or NULL.
err_ds_name <- function(s, i) {
  if (i > nrow(s) || s$type[i] != "name") return(NULL)
  nm <- tolower(s$value[i])
  i <- i + 1L
  if (i + 1L <= nrow(s) && s$type[i] == "dot" && s$type[i + 1L] == "name") {
    if (nm != "work") return(list(name = NA_character_, i = i + 2L))
    nm <- tolower(s$value[i + 1L])
    i <- i + 2L
  }
  if (i <= nrow(s) && s$type[i] == "lparen") {
    depth <- 0L
    while (i <= nrow(s)) {
      if (s$type[i] == "lparen") depth <- depth + 1L
      if (s$type[i] == "rparen") depth <- depth - 1L
      i <- i + 1L
      if (depth == 0L) break
    }
  }
  list(name = nm, i = i)
}

err_kw <- c("if", "then", "else", "do", "end", "to", "by", "while", "until", "and", "or", "not", "in", "eq",
            "ne", "lt", "le", "gt", "ge", "of")

# Output data sets of the step in `text` and the variables SAS has compiled
# when the step stops: list(kind, outputs = named list of variable names).
err_recover_step <- function(text, eng) {
  st <- err_statements(text)
  if (is.null(st)) return(list(kind = "unknown", outputs = list()))
  st <- Filter(function(s) nrow(s) > 0, st)
  if (!length(st)) return(list(kind = "unknown", outputs = list()))
  first <- st[[1]]
  kw <- tolower(first$value[1])
  if (kw == "proc") {
    outs <- list()
    i <- 3L
    while (i < nrow(first)) {
      if (first$type[i] == "name" && tolower(first$value[i]) == "out" && first$value[i + 1L] == "=") {
        d <- err_ds_name(first, i + 2L)
        if (!is.null(d) && !is.na(d$name)) outs[[d$name]] <- character()
        i <- if (is.null(d)) i + 2L else d$i
      } else {
        i <- i + 1L
      }
    }
    return(list(kind = "proc", name = tolower(first$value[2] %||% ""), outputs = outs))
  }
  if (kw != "data") return(list(kind = "unknown", outputs = list()))
  outs <- character()
  i <- 2L
  while (i <= nrow(first)) {
    d <- err_ds_name(first, i)
    if (is.null(d)) break
    if (!is.na(d$name) && d$name != "_null_") outs <- c(outs, d$name)
    i <- d$i
  }
  vars <- character()
  add <- function(v) vars <<- c(vars, setdiff(v, vars))
  for (s in st[-1]) {
    k <- tolower(s$value[1])
    if (k %in% c("run", "datalines", "cards")) break
    if (k %in% c("set", "merge")) {
      j <- 2L
      ds <- character()
      ok <- TRUE
      while (j <= nrow(s)) {
        if (s$type[j] == "name" && j < nrow(s) && s$value[j + 1L] == "=") {
          if (!tolower(s$value[j]) %in% c("end", "nobs", "point", "indsname", "curobs", "key")) ok <- FALSE
          j <- j + 3L
          next
        }
        d <- err_ds_name(s, j)
        if (is.null(d)) {
          ok <- FALSE
          break
        }
        if (!is.na(d$name)) ds <- c(ds, d$name)
        j <- d$i
      }
      if (ok) for (d in ds) if (!is.null(eng$meta[[d]])) add(eng$meta[[d]]$vars)
      next
    }
    if (nrow(s) >= 2L && s$type[1] == "name" && s$value[2] == "=" && !k %in% err_kw) {
      add(tolower(s$value[1]))
      if (nrow(s) > 2L) {
        r <- s[-(1:2), , drop = FALSE]
        nxt <- c(r$type[-1], "")
        nm <- r$type == "name" & !nxt %in% c("lparen", "lbrace", "lbracket", "dot") & !tolower(r$value) %in% err_kw
        add(tolower(r$value[nm]))
      }
    }
  }
  list(kind = "data", outputs = stats::setNames(rep(list(vars), length(outs)), outs))
}

err_set_macro <- function(eng, syserr = NULL, syscc = NULL) {
  g <- eng$ses$globals
  if (!is.null(syserr)) assign("syserr", as.character(syserr), envir = g)
  if (!is.null(syscc)) {
    old <- suppressWarnings(as.numeric(get0("syscc", envir = g, inherits = FALSE) %||% "0"))
    assign("syscc", as.character(max(old, syscc, na.rm = TRUE)), envir = g)
  }
}

# Handle a SAS ERROR raised while compiling or running the step in `text`.
engine_step_error <- function(eng, e, text) {
  if (identical(eng$phase, "execute_data")) {
    abort_unsupported(paste0("the data sets that an execution-time ERROR leaves in WORK (", conditionMessage(e), ")"),
                      e$sas_line %||% NULL)
  }
  eng$status <- "error"
  eng$errors <- c(eng$errors, conditionMessage(e))
  if (is.null(eng$error_line)) eng$error_line <- e$sas_line %||% NA_integer_
  eng$log <- c(eng$log, err_message_lines(e))
  if (isTRUE(eng$sql_open) || identical(eng$phase, "sql")) {
    if (!isTRUE(eng$sql_noexec)) {
      eng$log <- c(eng$log, "NOTE: PROC SQL set option NOEXEC and will continue to check the syntax of statements.")
    }
    eng$sql_noexec <- TRUE
    err_set_macro(eng, syscc = 1012)
    return(invisible())
  }
  rec <- err_recover_step(text, eng)
  eng$log <- c(eng$log, "NOTE: The SAS System stopped processing this step because of errors.")
  if (!isTRUE(eng$syntax_check) && !isTRUE(eng$nosyntaxcheck)) {
    eng$log <- c(eng$log, "NOTE: Due to ERROR(s) above, SAS set option OBS=0, enabling syntax check mode.",
                 "      This prevents execution of subsequent data modification statements.")
    eng$syntax_check <- TRUE
  }
  for (nm in names(rec$outputs)) {
    old <- get0(nm, envir = eng$wenv, inherits = FALSE)
    vars <- rec$outputs[[nm]]
    eng$log <- c(eng$log, sprintf(
      "WARNING: The data set WORK.%s may be incomplete.  When this step was stopped there were 0 observations and %d variables.",
      toupper(nm), length(vars)))
    if (!is.null(old)) {
      eng$log <- c(eng$log, sprintf("WARNING: Data set WORK.%s was not replaced because this step was stopped.", toupper(nm)))
    } else {
      engine_empty_dataset(eng, nm, vars)
    }
  }
  err_set_macro(eng, syserr = 1012, syscc = if (identical(rec$kind, "proc")) 3000 else 1012)
  invisible()
}

# A data set with 0 observations and the given (numeric unless known) variables.
engine_empty_dataset <- function(eng, nm, vars, types = NULL) {
  cols <- lapply(vars, function(v) if (identical(types[[v]], "char")) character() else numeric())
  d <- tibble::as_tibble(stats::setNames(cols, vars), .rows = 0L)
  assign(nm, d, envir = eng$wenv)
  eng$meta[[nm]] <- meta_from_df(d)
  invisible()
}
