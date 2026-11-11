# PROC SQL statements and their integration with the engine (WORK, metadata,
# log, listing, macro variables).

sql_note <- function(eng, lines) {
  if (isTRUE(rt$notes_on) && isTRUE(eng$step_notes)) eng$log <- c(eng$log, lines)
  invisible()
}

sql_set_mvar <- function(eng, name, value) mv_let(eng$ses, name, value)

sql_set_status <- function(eng, sqlobs, sqlrc = 0) {
  assign("sqlobs", as.character(sqlobs), envir = eng$ses$globals)
  assign("sqlrc", as.character(sqlrc), envir = eng$ses$globals)
}

# Execute one SQL statement.
engine_sql <- function(eng, st) {
  rt_reset()
  on.exit({
    # PROC SQL writes no "Missing values were generated" NOTE (gold standard)
    rt$mv <- integer()
    if (isTRUE(eng$step_notes)) sql_note(eng, step_end_notes())
  })
  switch(st$kind,
    select = sql_stmt_select(eng, st),
    create_table = sql_stmt_create(eng, st),
    create_view = {
      eng$views[[st$table]] <- st$query
      sql_note(eng, sprintf("NOTE: SQL view WORK.%s has been defined.", toupper(st$table)))
    },
    create_like = {
      src <- get0(st$source, envir = eng$wenv, inherits = FALSE)
      if (is.null(src)) abort_runtime(paste0("File WORK.", toupper(st$source), ".DATA does not exist."), st$line)
      sql_store(eng, st$table, src[0, , drop = FALSE], eng$meta[[st$source]] %||% meta_from_df(src))
      sql_note(eng, sprintf("NOTE: Table WORK.%s created, with 0 rows and %d columns.", toupper(st$table), ncol(src)))
    },
    create_cols = sql_stmt_create_cols(eng, st),
    insert_values = ,
    insert_set = ,
    insert_query = sql_stmt_insert(eng, st),
    update = sql_stmt_update(eng, st),
    delete = sql_stmt_delete(eng, st),
    drop = {
      for (t in st$tables) {
        if (st$what == "view") {
          eng$views[[t]] <- NULL
          sql_note(eng, sprintf("NOTE: View WORK.%s has been dropped.", toupper(t)))
          next
        }
        if (!exists(t, envir = eng$wenv, inherits = FALSE)) {
          sql_note(eng, sprintf("WARNING: File WORK.%s.DATA does not exist.", toupper(t)))
          next
        }
        rm(list = t, envir = eng$wenv)
        eng$meta[[t]] <- NULL
        sql_note(eng, sprintf("NOTE: Table WORK.%s has been dropped.", toupper(t)))
      }
    },
    alter = sql_stmt_alter(eng, st),
    reset = {
      o <- eng$sql_opts %||% list()
      for (nm in names(st$options)) {
        pair <- c(print = "noprint", noprint = "print", number = "nonumber", nonumber = "number")
        if (nm %in% names(pair)) o[[pair[[nm]]]] <- NULL
        o[[nm]] <- st$options[[nm]]
      }
      eng$sql_opts <- o
    },
    abort_unsupported(paste0("PROC SQL ", toupper(st$kind)), st$line)
  )
  invisible()
}

sql_outobs <- function(eng, res) {
  n <- eng$sql_opts$outobs
  if (!is.null(n) && nrow(res$data) > n) {
    res <- sql_rel_rows(res, seq_len(n))
    sql_note(eng, sprintf("WARNING: Statement terminated early due to OUTOBS=%d option.", as.integer(n)))
  }
  res
}

sql_stmt_select <- function(eng, st) {
  q <- st$query
  res <- sql_outobs(eng, sql_query(q, eng))
  n <- nrow(res$data)
  into <- if (q$node == "select") q$into else NULL
  if (n == 0) sql_note(eng, "NOTE: No rows were selected.")
  if (!is.null(into)) sql_into(eng, res, into, st$line)
  if (!isTRUE(eng$sql_opts$noprint) && n > 0) {
    eng$listing <- c(eng$listing, sas_sql_listing(res, eng$titles, number = isTRUE(eng$sql_opts$number)))
  }
  sql_set_status(eng, n)
}

# Text of a value for INTO: the column format, or BEST8. for numbers.
sql_into_text <- function(v, c) {
  if (c$type == "char") return(if (is.null(c$format)) v else sas_format(v, c$format))
  if (is.null(c$format)) sas_fmt_best(v, 8L) else sas_format(v, c$format)
}

sql_into <- function(eng, res, into, line) {
  if (length(into) > length(res$cols)) abort_syntax("The number of INTO host variables is greater than the number of columns.", line)
  n <- nrow(res$data)
  for (k in seq_along(into)) {
    tg <- into[[k]]
    c <- res$cols[[k]]
    vals <- sql_into_text(res$data[[c$id]], c)
    if (!is.null(tg$to) || isTRUE(tg$open)) {
      base <- split_trailing_digits(tg$name)
      from <- if (is.na(base$width)) 1L else base$width
      if (isTRUE(tg$open)) {
        names <- paste0(base$base, seq_len(n) + from - 1L)
      } else {
        to <- split_trailing_digits(tg$to)
        names <- paste0(base$base, seq(from, to$width))
      }
      m <- min(length(names), n)
      for (j in seq_len(m)) {
        v <- vals[j]
        sql_set_mvar(eng, names[j], if (isTRUE(tg$notrim)) v else trimws(v))
      }
      next
    }
    if (n == 0) next
    if (!is.null(tg$sep)) {
      vv <- if (isTRUE(tg$notrim)) vals else trimws(vals)
      sql_set_mvar(eng, tg$name, paste(vv, collapse = tg$sep))
      next
    }
    v <- vals[1]
    if (isTRUE(tg$trimmed)) v <- trimws(v)
    sql_set_mvar(eng, tg$name, v)
  }
}

# Store a relation's rows as a WORK data set.
sql_rel_to_table <- function(eng, res, line, warn_dups = TRUE) {
  cols <- res$cols
  names <- vapply(cols, `[[`, "", "name")
  tema <- 0L
  for (k in seq_along(cols)) {
    if (!nzchar(names[k])) {
      tema <- tema + 1L
      names[k] <- sprintf("_tema%03d", tema)
      cols[[k]]$disp <- toupper(names[k])
    }
  }
  keep <- !duplicated(names)
  for (k in which(!keep)) {
    if (warn_dups) sql_note(eng, sprintf("WARNING: Variable %s already exists on file WORK.", cols[[k]]$disp))
  }
  cols <- cols[keep]
  names <- names[keep]
  d <- res$data[vapply(cols, `[[`, "", "id")]
  names(d) <- names
  for (k in seq_along(cols)) if (cols[[k]]$type == "char") d[[names[k]]] <- sas_char(d[[names[k]]], cols[[k]]$len)
  meta <- list(
    vars = names,
    type = stats::setNames(lapply(cols, `[[`, "type"), names),
    len = stats::setNames(lapply(cols, function(c) as.integer(c$len)), names),
    formats = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "format"), names)),
    informats = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "informat"), names)),
    labels = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "label"), names)),
    disp = stats::setNames(lapply(cols, `[[`, "disp"), names)
  )
  list(data = tibble::as_tibble(d), meta = meta)
}

sql_store <- function(eng, name, d, meta, ordered = TRUE) {
  assign(name, tibble::as_tibble(d), envir = eng$wenv)
  eng$meta[[name]] <- meta
  eng$last <- name
  eng$unordered <- if (ordered) setdiff(eng$unordered, name) else union(eng$unordered, name)
  invisible()
}

sql_stmt_create <- function(eng, st) {
  res <- sql_outobs(eng, sql_query(st$query, eng))
  tb <- sql_rel_to_table(eng, res, st$line)
  sql_store(eng, st$table, tb$data, tb$meta, ordered = isTRUE(res$ordered))
  sql_note(eng, sprintf("NOTE: Table WORK.%s created, with %d rows and %d columns.", toupper(st$table), nrow(tb$data), ncol(tb$data)))
  sql_set_status(eng, nrow(tb$data))
}

sql_stmt_create_cols <- function(eng, st) {
  d <- list()
  meta <- list(vars = character(), type = list(), len = list(), formats = list(), informats = list(), labels = list(), disp = list())
  for (c in st$cols) {
    nm <- c$name
    d[[nm]] <- if (c$type == "char") character() else numeric()
    meta$vars <- c(meta$vars, nm)
    meta$type[[nm]] <- c$type
    meta$len[[nm]] <- as.integer(c$len)
    meta$disp[[nm]] <- c$text
    if (!is.null(c$format)) meta$formats[[nm]] <- c$format
    if (!is.null(c$informat)) meta$informats[[nm]] <- c$informat
    if (!is.null(c$label)) meta$labels[[nm]] <- c$label
  }
  sql_store(eng, st$table, tibble::as_tibble(d), meta)
  sql_note(eng, sprintf("NOTE: Table WORK.%s created, with 0 rows and %d columns.", toupper(st$table), length(st$cols)))
  sql_set_status(eng, 0)
}

sql_target <- function(eng, name, line) {
  d <- get0(name, envir = eng$wenv, inherits = FALSE)
  if (is.null(d)) abort_runtime(paste0("File WORK.", toupper(name), ".DATA does not exist."), line)
  list(data = d, meta = eng$meta[[name]] %||% meta_from_df(d))
}

# Coerce new values to a target column.
sql_coerce_col <- function(v, type, len, name, line) {
  if (type == "char") {
    if (!is.character(v)) abort_syntax(paste0("Value for column ", name, " is not character."), line)
    return(sas_char(substr(v, 1L, len), len))
  }
  if (is.character(v)) abort_syntax(paste0("Value for column ", name, " is not numeric."), line)
  as.numeric(v)
}

sql_stmt_insert <- function(eng, st) {
  tg <- sql_target(eng, st$table, st$line)
  m <- tg$meta
  vars <- names(tg$data)
  if (st$kind == "insert_query") {
    res <- sql_query(st$query, eng)
    cols <- st$cols %||% vars
    if (length(res$cols) != length(cols)) abort_syntax("The number of columns in the query does not match the INSERT column list.", st$line)
    new <- lapply(seq_along(cols), function(k) res$data[[res$cols[[k]]$id]])
    names(new) <- cols
    nrows <- nrow(res$data)
  } else {
    rows <- st$rows
    ctx <- sql_ctx(eng, sql_empty_rel(1L))
    new <- list()
    for (r in rows) {
      cols <- if (st$kind == "insert_set") names(r) else (st$cols %||% vars)
      vals <- if (st$kind == "insert_set") unname(r) else r
      if (length(vals) != length(cols)) abort_syntax("VALUES clause has a different number of values than columns.", st$line)
      for (k in seq_along(cols)) new[[cols[k]]] <- c(new[[cols[k]]], list(sql_eval(vals[[k]], ctx)))
    }
    nrows <- length(rows)
    new <- lapply(new, function(x) unlist(x))
  }
  miss <- setdiff(names(new), vars)
  if (length(miss) > 0) abort_syntax(paste0("Column ", miss[1], " could not be found in the table/view identified with the correlation name ", toupper(st$table), "."), st$line)
  add <- list()
  for (v in vars) {
    type <- m$type[[v]] %||% (if (is.character(tg$data[[v]])) "char" else "num")
    len <- m$len[[v]] %||% 8L
    add[[v]] <- if (v %in% names(new)) {
      x <- new[[v]]
      if (st$kind == "insert_set") x <- rep_len(x, nrows)
      sql_coerce_col(x, type, len, v, st$line)
    } else if (type == "char") rep(strrep(" ", len), nrows) else rep(NA_real_, nrows)
  }
  d <- dplyr::bind_rows(tg$data, tibble::as_tibble(add, .rows = nrows))
  sql_store(eng, st$table, d, m, ordered = !st$table %in% eng$unordered)
  sql_note(eng, sprintf("NOTE: %d %s inserted into WORK.%s.", nrows, if (nrows == 1) "row was" else "rows were", toupper(st$table)))
  sql_set_status(eng, nrows)
}

sql_target_rel <- function(eng, st) {
  src <- list(kind = "table", name = st$table, alias = st$alias, options = list(), line = st$line)
  sql_table_rel(src, eng, NULL)
}

sql_stmt_update <- function(eng, st) {
  tg <- sql_target(eng, st$table, st$line)
  rel <- sql_target_rel(eng, st)
  ctx <- sql_ctx(eng, rel)
  hit <- if (is.null(st$where)) rep(TRUE, nrow(rel$data)) else sas_true(sql_eval(st$where, ctx))
  idx <- which(hit)
  d <- tg$data
  if (length(idx) > 0) {
    sub <- sql_rel_rows(rel, idx)
    sctx <- sql_ctx(eng, sub)
    vals <- lapply(st$sets, function(s) sql_eval(s$expr, sctx))
    for (k in seq_along(st$sets)) {
      v <- st$sets[[k]]$name
      if (!v %in% names(d)) abort_syntax(paste0("Column ", v, " could not be found in the table/view identified with the correlation name ", toupper(st$table), "."), st$line)
      type <- tg$meta$type[[v]] %||% (if (is.character(d[[v]])) "char" else "num")
      d[[v]][idx] <- sql_coerce_col(vals[[k]], type, tg$meta$len[[v]] %||% 8L, v, st$line)
    }
  }
  sql_store(eng, st$table, d, tg$meta, ordered = !st$table %in% eng$unordered)
  sql_note(eng, sprintf("NOTE: %d %s updated in WORK.%s.", length(idx), if (length(idx) == 1) "row was" else "rows were", toupper(st$table)))
  sql_set_status(eng, length(idx))
}

sql_stmt_delete <- function(eng, st) {
  tg <- sql_target(eng, st$table, st$line)
  rel <- sql_target_rel(eng, st)
  hit <- if (is.null(st$where)) rep(TRUE, nrow(rel$data)) else sas_true(sql_eval(st$where, sql_ctx(eng, rel)))
  d <- tg$data[!hit, , drop = FALSE]
  k <- sum(hit)
  sql_store(eng, st$table, d, tg$meta, ordered = !st$table %in% eng$unordered)
  sql_note(eng, sprintf("NOTE: %d %s deleted from WORK.%s.", k, if (k == 1) "row was" else "rows were", toupper(st$table)))
  sql_set_status(eng, k)
}

sql_stmt_alter <- function(eng, st) {
  tg <- sql_target(eng, st$table, st$line)
  d <- tg$data
  m <- tg$meta
  for (a in st$actions) {
    if (a$op == "add") {
      c <- a$col
      if (c$name %in% names(d)) abort_syntax(paste0("Column ", c$name, " already exists in table WORK.", toupper(st$table), "."), st$line)
      d[[c$name]] <- if (c$type == "char") rep(strrep(" ", c$len), nrow(d)) else rep(NA_real_, nrow(d))
      m$vars <- c(m$vars, c$name)
      m$type[[c$name]] <- c$type
      m$len[[c$name]] <- as.integer(c$len)
      m$disp[[c$name]] <- c$text
      if (!is.null(c$format)) m$formats[[c$name]] <- c$format
      if (!is.null(c$informat)) m$informats[[c$name]] <- c$informat
      if (!is.null(c$label)) m$labels[[c$name]] <- c$label
    } else if (a$op == "drop") {
      if (!a$name %in% names(d)) abort_syntax(paste0("Column ", a$name, " could not be found in table WORK.", toupper(st$table), "."), st$line)
      d[[a$name]] <- NULL
      m$vars <- setdiff(m$vars, a$name)
      for (f in c("type", "len", "formats", "informats", "labels", "disp")) m[[f]][[a$name]] <- NULL
    } else {
      c <- a$col
      v <- c$name
      if (!v %in% names(d)) abort_syntax(paste0("Column ", v, " could not be found in table WORK.", toupper(st$table), "."), st$line)
      if (!is.null(c$type) && c$type != m$type[[v]]) abort_unsupported("changing the type of a column with ALTER TABLE MODIFY", st$line)
      if (!is.null(c$len) && identical(m$type[[v]], "char")) {
        d[[v]] <- sas_char(substr(d[[v]], 1L, c$len), c$len)
        m$len[[v]] <- as.integer(c$len)
      }
      if (!is.null(c$format)) m$formats[[v]] <- c$format
      if (!is.null(c$informat)) m$informats[[v]] <- c$informat
      if (!is.null(c$label)) m$labels[[v]] <- c$label
    }
  }
  sql_store(eng, st$table, d, m, ordered = !st$table %in% eng$unordered)
  sql_note(eng, sprintf("NOTE: Table WORK.%s has been modified, with %d columns.", toupper(st$table), ncol(d)))
}
