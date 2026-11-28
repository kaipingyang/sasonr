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

# A numeric option value: a number token, optionally after a minus sign.
# `int = TRUE`: an integer constant is required, so `2.5` or a name is a
# syntax error (not truncated to 2).
parse_opt_number <- function(p, line, int = FALSE) {
  neg <- is_op(p, "-") && tk_type(p, 1L) == "num"
  if (neg) advance(p)
  if (int && !tk_int(p)) abort_syntax("Expecting an integer constant.", line)
  if (tk_type(p) != "num") {
    abort_syntax("expecting one of the following: a numeric constant, a datetime constant.", line)
  }
  v <- as.numeric(tk_val(p))
  advance(p)
  if (neg) -v else v
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
          f <- sp$fmt
          if (!is.null(f) && identical(st$info[[v]]$type, "char") && !isTRUE(f$char) &&
              is.null(user_fmt(f$name, FALSE)) && !is.null(user_fmt(f$name, TRUE))) {
            abort_runtime(paste0("The format ", toupper(f$name), " was not found or could not be loaded."), s$line)
          }
          if (is.null(f)) st$formats[[v]] <- NULL else st$formats[[v]] <- f
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
         len = if (char) vctrs::vec_cast(inf$len, integer()) else vctrs::vec_cast(st$numlen[[v]] %||% 8L, integer()),
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

# Class levels outside the data (CLASSDATA= values, PRELOADFMT ranges), shared
# by PROC TABULATE and (later) PROC MEANS COMPLETETYPES (spec 10 D4): added to
# the level set in internal order; `extra` marks them (no row has them).
proc_levels_extend <- function(l, extra_values, info) {
  if (!length(extra_values)) return(l)
  k <- proc_group_key(extra_values, info)
  new <- !k %in% l$keys & !duplicated(k)
  if (!any(new)) return(l)
  vals <- vctrs::vec_c(l$values, extra_values[new])
  keys <- c(l$keys, k[new])
  ord <- proc_internal_order(vals)
  l$values <- vals[ord]
  l$keys <- keys[ord]
  l$extra <- keys[ord] %in% k[new]
  l$idx <- match(l$key, l$keys)
  l
}

# The values a user format's ranges give (PRELOADFMT): discrete values only.
proc_preload_values <- function(fmt, char, line) {
  s <- sas_rt_active()
  def <- s$formats[[paste0("value:", if (char) "$", fmt$name)]]
  if (is.null(def)) abort_unsupported(paste0("PRELOADFMT with the format ", toupper(fmt$name), " that is not a user format"), line)
  vapply(def$entries, function(e) {
    lo <- e[["lo"]]
    hi <- e[["hi"]]
    if (isTRUE(e[["other"]]) || isTRUE(e[["lo_low"]]) || isTRUE(e[["hi_high"]]) || (!is.null(hi) && !identical(hi, lo)) ||
        isTRUE(e[["lo_excl"]]) || isTRUE(e[["hi_excl"]]) || is.null(lo)) {
      abort_unsupported("PRELOADFMT with a format range that is not one value", line)
    }
    if (char) as.character(lo) else as.numeric(lo)
  }, if (char) "" else 0)
}

# BY groups ---------------------------------------------------------------------

# Row index vectors of the BY groups of `d` (data order); stops with SAS's
# error when the data are not sorted.
proc_by_groups <- function(df, by, descending, dsname, info) {
  n <- nrow(df)
  if (length(by) == 0 || n == 0) return(if (n == 0 && length(by) > 0) list() else list(seq_len(n)))
  keys <- lapply(by, function(v) proc_group_key(df[[v]], info[[v]]))
  if (n > 1) {
    res <- integer(n - 1L)
    for (k in seq_along(by)) {
      x <- df[[by[k]]]
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
        paste0(info[[by[k]]]$disp, " = ", proc_value_text(df[[by[k]]][r], info[[by[k]]]))
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
proc_by_line <- function(df, r, by, info) {
  paste(vapply(by, function(v) paste0(info[[v]]$disp, "=", proc_value_text(df[[v]][r], info[[v]])), ""), collapse = " ")
}

# Output data sets ----------------------------------------------------------------

# Metadata for an output data set from column specs list(name, char, len, fmt,
# informat, label, disp).
proc_out_meta <- function(cols) {
  vars <- vapply(cols, `[[`, "", "name")
  m <- list(
    vars = vars,
    type = stats::setNames(lapply(cols, function(c) if (c$char) "char" else "num"), vars),
    len = stats::setNames(lapply(cols, function(c) vctrs::vec_cast(c$len %||% 8L, integer())), vars),
    formats = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "fmt"), vars)),
    informats = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "informat"), vars)),
    labels = Filter(Negate(is.null), stats::setNames(lapply(cols, `[[`, "label"), vars)),
    disp = stats::setNames(lapply(cols, function(c) c$disp %||% c$name), vars)
  )
  m
}

#' Store PROC output data sets in WORK; returns their metadata
#' @param work The WORK environment.
#' @param outs Named list of `list(data, meta)`.
#' @export
#' @keywords internal
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

# Split a column heading. `parts` are the preferred split points (e.g.
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
  widths <- vapply(cols, function(c) vctrs::vec_cast(c$width, integer()), 1L)
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

table_width <- function(cols, gap) sum(vapply(cols, function(c) vctrs::vec_cast(c$width, integer()), 1L)) + gap * (length(cols) - 1L)

# Numeric values are right aligned unless a user-defined format turns them into
# labels.
proc_right_align <- function(info) {
  !info$char && (is.null(info$fmt) || is.null(user_fmt(info$fmt$name, FALSE)))
}
