# Shared helpers for the analytic PROCs (MEANS/SUMMARY, FREQ, TRANSPOSE,
# CONTENTS, DATASETS): statement parsing, BY groups, value display, output
# data set metadata and listing tables.

# Parsing -----------------------------------------------------------------------

# Parse a PROC body. `handler(p, kw, line)` parses one statement (the parser is
# on its keyword) and returns it. TITLE statements are collected separately;
# RUN; (unless `run_ends = FALSE`) and QUIT; end the step.
proc_parse_body <- function(p, pname, handler, run_ends = TRUE) {
  stmts <- list()
  titles <- list()
  repeat {
    if (proc_end_reached(p)) break
    if (tk_type(p) == "semi") {
      advance(p)
      next
    }
    if (is_kw(p, "quit") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      break
    }
    if (is_kw(p, "run") && tk_type(p, 1L) == "semi") {
      advance(p, 2L)
      if (run_ends) break
      stmts <- c(stmts, list(list(stmt = "run", line = tk_line(p, -1L))))
      next
    }
    sline <- tk_line(p)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC", toupper(pname)), sline)
    low <- tk_low(p)
    if (grepl("^title[0-9]*$", low)) {
      titles <- c(titles, list(parse_global_stmt(p)))
      next
    }
    s <- handler(p, low, sline)
    if (!is.null(s)) stmts <- c(stmts, list(s))
  }
  list(stmts = stmts, titles = titles)
}

# `kw item item ... ;` -> variable list items
parse_items_stmt <- function(p, what) {
  line <- tk_line(p)
  advance(p)
  items <- list()
  while (tk_type(p) == "name") items <- c(items, list(parse_varlist_item(p)))
  if (tk_type(p) != "semi") abort_syntax(paste("unexpected", tok_desc(p), "in", toupper(what), "statement"), line)
  advance(p)
  list(stmt = what, items = items, line = line)
}

# The value after `option=`: a name, number or quoted string (as text).
parse_opt_value <- function(p, opt, line) {
  t <- tk_type(p)
  if (!t %in% c("name", "num", "str", "str_macro")) abort_syntax(paste0("expected a value after ", toupper(opt), "="), line)
  v <- tk_val(p)
  advance(p)
  # e.g. ALPHA=.05 or ALPHA=0.1 are single tokens; a negative number is two
  v
}

# Resolve variable list items against an input PDV (the analysed state).
proc_resolve <- function(items, st, line) {
  tmp <- new_state(list())
  tmp$order <- st$order
  tmp$info <- st$info
  vars <- resolve_items(items, tmp, line, register = FALSE)
  miss <- setdiff(vars, st$order)
  if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found."), line)
  as.character(vars)
}

# Apply FORMAT / LABEL / ATTRIB statements given inside a PROC step to the
# (temporary) attributes of its input.
proc_apply_attr_stmts <- function(stmts, st) {
  for (s in stmts) {
    if (identical(s$stmt, "format")) {
      for (sp in s$specs) {
        vars <- proc_resolve(sp$vars, st, s$line)
        for (v in vars) {
          if (is.null(sp$fmt)) st$formats[[v]] <- NULL else st$formats[[v]] <- sp$fmt
        }
      }
    } else if (identical(s$stmt, "label")) {
      for (v in names(s$labels)) {
        if (!v %in% st$order) abort_runtime(paste0("Variable ", toupper(v), " not found."), s$line)
        st$labels[v] <- s$labels[[v]]
      }
    } else if (identical(s$stmt, "attrib")) {
      for (g in s$groups) {
        vars <- proc_resolve(g$vars, st, s$line)
        for (v in vars) {
          if (!is.null(g$format)) st$formats[[v]] <- g$format
          if (!is.null(g$label)) st$labels[v] <- g$label
          if (!is.null(g$len) || !is.null(g$informat)) abort_unsupported("ATTRIB LENGTH=/INFORMAT= in a PROC step", s$line)
        }
      }
    }
  }
  invisible(st)
}

# Attributes of input variables as a plain list for the run-time spec.
proc_var_info <- function(st, vars) {
  out <- lapply(vars, function(v) {
    inf <- st$info[[v]]
    char <- identical(inf$type, "char")
    f <- st$formats[[v]]
    list(name = v, disp = inf$disp %||% v, char = char,
         len = if (char) as.integer(inf$len) else as.integer(st$numlen[[v]] %||% 8L),
         fmt = if (is.null(f)) NULL else f[c("name", "w", "d", "char", "text")],
         informat = if (is.null(st$informats[[v]])) NULL else st$informats[[v]][c("name", "w", "d", "char", "text")],
         label = if (v %in% names(st$labels)) unname(st$labels[[v]]) else NULL)
  })
  names(out) <- vars
  out
}

# Values ------------------------------------------------------------------------

# Display text of values (trimmed): formatted, else BEST12. / character value.
proc_value_text <- function(x, info) {
  if (is.character(x)) {
    s <- if (is.null(info$fmt)) x else sas_format(x, info$fmt)
    return(trimws(sas_rtrim(s), which = "left") |> sas_rtrim())
  }
  trimws(sas_format(x, info$fmt))
}

# Grouping key of values: formatted text when a format is attached, else the
# value itself (character values without trailing blanks; numeric missing
# values keep their special-missing identity).
proc_group_key <- function(x, info) {
  if (!is.null(info$fmt)) return(proc_value_text(x, info))
  if (is.character(x)) return(sas_rtrim(x))
  code <- sas_miss_code(x)
  ifelse(is.na(code), format(x, digits = 17, scientific = TRUE, trim = TRUE), paste0("\001miss", code))
}

# Lowest internal value first (missing values first: ._ . .A ... .Z).
proc_internal_order <- function(x) {
  if (length(x) == 0) return(integer())
  d <- list(v = x)
  sas_order(tibble::as_tibble(d), "v")
}

# BY groups ---------------------------------------------------------------------

# Row index vectors of the BY groups of `d` (data order); stops with SAS's
# error when the data are not sorted.
proc_by_groups <- function(d, by, descending, dsname, info) {
  n <- nrow(d)
  if (length(by) == 0 || n == 0) return(if (n == 0 && length(by) > 0) list() else list(seq_len(n)))
  keys <- lapply(by, function(v) proc_group_key(d[[v]], info[[v]]))
  if (n > 1) {
    res <- integer(n - 1L)
    for (k in seq_along(by)) {
      x <- d[[by[k]]]
      ck <- sas_cmp(x[-n], x[-1L])
      # equal formatted values form one group whatever the internal order
      ck[keys[[k]][-n] == keys[[k]][-1L]] <- 0L
      if (descending[k]) ck <- -ck
      res <- ifelse(res == 0L, ck, res)
    }
    bad <- which(res > 0L)
    if (length(bad) > 0) {
      i <- bad[1]
      desc <- function(r) paste(vapply(seq_along(by), function(k) {
        paste0(info[[by[k]]]$disp, " = ", proc_value_text(d[[by[k]]][r], info[[by[k]]]))
      }, ""), collapse = " ")
      abort_runtime(paste0("Data set WORK.", toupper(dsname), " is not sorted in ",
                           if (all(descending)) "descending" else "ascending", " sequence. The current BY group has ",
                           desc(i), " and the next BY group has ", desc(i + 1L), "."))
    }
  }
  chg <- rep(FALSE, n)
  chg[1] <- TRUE
  for (k in seq_along(keys)) chg[-1L] <- chg[-1L] | keys[[k]][-1L] != keys[[k]][-n]
  id <- cumsum(chg)
  unname(split(seq_len(n), id))
}

# "g=1 h=A": the BY line of a BY group (first row `r`).
proc_by_line <- function(d, r, by, info) {
  paste(vapply(by, function(v) paste0(info[[v]]$disp, "=", proc_value_text(d[[v]][r], info[[v]])), ""), collapse = " ")
}

# Output data sets ----------------------------------------------------------------

# Metadata for an output data set from column specs list(name, char, len, fmt,
# informat, label, disp).
proc_out_meta <- function(cols) {
  vars <- vapply(cols, `[[`, "", "name")
  m <- list(
    vars = vars,
    type = stats::setNames(lapply(cols, function(c) if (c$char) "char" else "num"), vars),
    len = stats::setNames(lapply(cols, function(c) as.integer(c$len %||% 8L)), vars),
    formats = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "fmt"), vars)),
    informats = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "informat"), vars)),
    labels = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "label"), vars)),
    disp = stats::setNames(lapply(cols, function(c) c$disp %||% c$name), vars)
  )
  m
}

# KEEP= / DROP= / RENAME= on an output data set (data + metadata).
proc_out_options <- function(d, m, opts, line = NULL) {
  if (length(opts) == 0) return(list(data = d, meta = m))
  vars <- m$vars
  if (!is.null(opts$where)) abort_unsupported("WHERE= on an output data set", line)
  keep <- vars
  if (!is.null(opts$keep)) keep <- intersect(vars, resolve_ds_items(opts$keep, vars, line))
  if (!is.null(opts$drop)) keep <- setdiff(keep, resolve_ds_items(opts$drop, vars, line))
  for (o in c("keep", "drop")) {
    if (is.null(opts[[o]])) next
    miss <- setdiff(resolve_ds_items(opts[[o]], vars, line), vars)
    if (length(miss) > 0) {
      abort_runtime(paste0("The variable ", toupper(miss[1]), " in the DROP, KEEP, or RENAME list has never been referenced."), line)
    }
  }
  d <- d[keep]
  sub <- function(l) l[intersect(names(l), keep)]
  m <- list(vars = keep, type = m$type[keep], len = m$len[keep], formats = sub(m$formats), informats = sub(m$informats),
            labels = sub(m$labels), disp = m$disp[keep])
  if (!is.null(opts$rename)) {
    rn <- opts$rename
    hit <- match(names(rn), keep)
    if (anyNA(hit)) {
      abort_runtime(paste0("The variable ", toupper(names(rn)[is.na(hit)][1]),
                           " in the DROP, KEEP, or RENAME list has never been referenced."), line)
    }
    new <- keep
    new[hit] <- unname(rn)
    ren <- function(l) {
      if (length(l) == 0) return(l)
      names(l) <- new[match(names(l), keep)]
      l
    }
    names(d) <- new
    disp <- m$disp
    disp[hit] <- as.list(unname(rn))
    m <- list(vars = new, type = ren(m$type), len = ren(m$len), formats = ren(m$formats), informats = ren(m$informats),
              labels = ren(m$labels), disp = stats::setNames(disp, new))
  }
  list(data = d, meta = m)
}

#' Store PROC output data sets in WORK; returns their metadata
#' @param work The WORK environment.
#' @param outs Named list of `list(data, meta)`.
#' @export
sas_proc_store <- function(work, outs) {
  meta <- list()
  for (nm in names(outs)) {
    d <- outs[[nm]]$data
    m <- outs[[nm]]$meta
    for (v in names(d)) {
      if (is.character(d[[v]])) d[[v]] <- sas_char(d[[v]], m$len[[v]])
    }
    assign(nm, tibble::as_tibble(d), envir = work)
    meta[[nm]] <- m
  }
  meta
}

# Listing tables --------------------------------------------------------------------

# Split a column heading. `parts` are SAS's preferred split points (e.g.
# c("N", "Obs")); the heading is one line when it fits `width`.
heading_lines <- function(parts, width) {
  full <- paste(parts, collapse = " ")
  if (nchar(full) <= width || length(parts) == 1) return(full)
  # greedy fill of the predefined parts
  out <- character()
  cur <- ""
  for (pt in parts) {
    cand <- if (nzchar(cur)) paste(cur, pt) else pt
    if (nchar(cand) <= width || !nzchar(cur)) {
      cur <- cand
    } else {
      out <- c(out, cur)
      cur <- pt
    }
  }
  c(out, cur)
}

# Render a table: `cols` is a list of list(parts, width, right, cells). The
# heading lines are bottom aligned; a dashed rule (the table width) frames the
# body. `gap` blanks separate the columns.
render_table <- function(cols, gap = 4L, rule = TRUE) {
  heads <- lapply(cols, function(c) heading_lines(c$parts, c$width))
  nh <- max(vapply(heads, length, 1L))
  heads <- lapply(heads, function(h) c(rep("", nh - length(h)), h))
  widths <- vapply(cols, function(c) as.integer(c$width), 1L)
  pad <- function(x, w, r) formatC(x, width = if (r) w else -w)
  line_of <- function(vals) {
    sas_rtrim(paste(mapply(pad, vals, widths, vapply(cols, `[[`, TRUE, "right")), collapse = strrep(" ", gap)))
  }
  hl <- vapply(seq_len(nh), function(j) line_of(vapply(heads, `[`, "", j)), "")
  n <- length(cols[[1]]$cells)
  body <- if (n > 0) vapply(seq_len(n), function(i) line_of(vapply(cols, function(c) c$cells[i], "")), "") else character()
  total <- sum(widths) + gap * (length(widths) - 1L)
  if (rule) c(hl, strrep("-", total), body, strrep("-", total)) else c(hl, body)
}

table_width <- function(cols, gap) sum(vapply(cols, function(c) as.integer(c$width), 1L)) + gap * (length(cols) - 1L)
