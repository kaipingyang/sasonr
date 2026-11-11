# PROC CONTENTS and PROC DATASETS (basic): data set descriptors, the
# variable attribute listing, OUT= and the DATASETS member/attribute
# management statements (DELETE, CHANGE, MODIFY, APPEND, CONTENTS, KILL).

# Shared ---------------------------------------------------------------------------

contents_fmt_text <- function(f) {
  if (is.null(f)) return("")
  toupper(f$text)
}

# Parse a CONTENTS statement / PROC CONTENTS options (until `;`).
parse_contents_opts <- function(p, line, what) {
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax(paste("missing semicolon after", what), line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in", what), line)
    o <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      if (o %in% c("data", "out")) {
        if (o == "data" && is_kw(p, "_all_")) abort_unsupported("CONTENTS DATA=_ALL_", oline)
        opts[[o]] <- parse_ds_ref(p, if (o == "data") "input" else "output")
        if (o == "data" && opts$data$name == "_all_") abort_unsupported("CONTENTS DATA=_ALL_", oline)
        next
      }
      v <- tolower(parse_opt_value(p, o, oline))
      if (o == "order") {
        if (!v %in% c("varnum", "ignorecase", "collate", "casecollate")) abort_syntax(paste0("Invalid ORDER= value ", toupper(v)), oline)
        opts$order <- v
      } else if (o == "memtype") {
        if (v != "data") abort_unsupported(paste0("MEMTYPE=", toupper(v)), oline)
      } else {
        abort_unsupported(paste0(what, " option ", toupper(o), "="), oline)
      }
      next
    }
    if (!o %in% c("noprint", "varnum", "short", "position", "details", "nodetails", "nods", "centiles")) {
      abort_unsupported(paste0(what, " option ", toupper(o)), oline)
    }
    if (o == "centiles") abort_unsupported(paste0(what, " option CENTILES"), oline)
    opts[[o]] <- TRUE
    advance(p)
  }
  advance(p)
  opts
}

parse_proc_contents <- function(p, line) {
  advance(p)
  opts <- parse_contents_opts(p, line, "PROC CONTENTS")
  body <- proc_parse_body(p, "contents", function(p, kw, sline) {
    abort_syntax(paste0("Statement ", toupper(kw), " is not valid in PROC CONTENTS"), sline)
  })
  list(type = "proc", name = "contents", line = line, options = opts, stmts = body$stmts, titles = body$titles)
}

# Rows of the variable table (attributes of one member).
contents_vars <- function(m) {
  vars <- m$vars
  disp <- vapply(vars, function(v) m$disp[[v]] %||% v, "")
  type <- vapply(vars, function(v) m$type[[v]], "")
  len <- vapply(vars, function(v) as.integer(m$len[[v]] %||% 8L), 1L)
  list(vars = vars, disp = unname(disp), type = unname(type), len = unname(len),
       fmt = unname(vapply(vars, function(v) contents_fmt_text(m$formats[[v]]), "")),
       infmt = unname(vapply(vars, function(v) contents_fmt_text(m$informats[[v]]), "")),
       label = unname(vapply(vars, function(v) m$labels[[v]] %||% "", "")))
}

# Byte offsets in the observation buffer: numeric variables first, then
# character variables, each group in creation order.
contents_npos <- function(cv) {
  pos <- integer(length(cv$vars))
  at <- 0L
  for (k in c(which(cv$type == "num"), which(cv$type == "char"))) {
    pos[k] <- at
    at <- at + cv$len[k]
  }
  list(npos = pos, obslen = at)
}

contents_alpha_order <- function(disp, how = "ascii") {
  if (how %in% c("ignorecase", "casecollate")) order(toupper(disp), disp, method = "radix") else order(disp, method = "radix")
}

contents_var_table <- function(cv, idx, title) {
  num <- as.character(idx)
  cols <- list(
    list(parts = "#", width = max(1L, nchar(num)), right = FALSE, cells = num),
    list(parts = "Variable", width = max(8L, nchar(cv$disp[idx])), right = FALSE, cells = cv$disp[idx]),
    list(parts = "Type", width = 4L, right = FALSE, cells = ifelse(cv$type[idx] == "char", "Char", "Num")),
    list(parts = "Len", width = 3L, right = TRUE, cells = as.character(cv$len[idx]))
  )
  if (any(nzchar(cv$fmt))) cols <- c(cols, list(list(parts = "Format", width = max(6L, nchar(cv$fmt[idx])), right = FALSE, cells = cv$fmt[idx])))
  if (any(nzchar(cv$infmt))) cols <- c(cols, list(list(parts = "Informat", width = max(8L, nchar(cv$infmt[idx])), right = FALSE, cells = cv$infmt[idx])))
  if (any(nzchar(cv$label))) cols <- c(cols, list(list(parts = "Label", width = max(5L, nchar(cv$label[idx])), right = FALSE, cells = cv$label[idx])))
  tb <- render_table(cols, gap = 4L, rule = FALSE)
  c(title, "", tb, "")
}

#' PROC CONTENTS listing and OUT= data set
#' @param work The WORK environment.
#' @param name Member name.
#' @param m Member metadata.
#' @param sp Options.
#' @export
sas_proc_contents <- function(work, name, m, sp) {
  d <- work[[name]]
  if (is.null(d)) abort_runtime(paste0("File WORK.", toupper(name), ".DATA does not exist."))
  nobs <- nrow(d)
  cv <- contents_vars(m)
  np <- contents_npos(cv)
  n <- length(cv$vars)
  lst <- character()
  if (!isTRUE(sp$noprint)) {
    now <- format(Sys.time(), "%m/%d/%Y %H:%M:%S")
    sorted <- if (!is.null(m$sortedby)) "YES" else "NO"
    # numeric variables come first; the record is padded to a multiple of 8
    # when it holds an 8-byte numeric (shorter numerics and characters are not aligned)
    obslen <- if (any(cv$type == "num" & cv$len == 8L)) as.integer(ceiling(np$obslen / 8) * 8) else np$obslen
    two <- function(a, av, b, bv) sprintf("%-21s%-56s%-22s%s", a, av, b, bv)
    lst <- c(paste0("The ", sp$proc %||% "CONTENTS", " Procedure"), "",
             two("Data Set Name", paste0("WORK.", toupper(name)), "Observations", nobs),
             two("Member Type", "DATA", "Variables", n),
             two("Engine", "V9", "Indexes", 0),
             two("Created", now, "Observation Length", obslen),
             two("Last Modified", now, "Deleted Observations", 0),
             two("Protection", "", "Compressed", "NO"),
             two("Data Set Type", m$typemem %||% "", "Sorted", sorted),
             two("Label", m$memlabel %||% "", "", ""),
             "Data Representation  SOLARIS_X86_64, LINUX_X86_64, ALPHA_TRU64, LINUX_IA64",
             "Encoding             utf-8  Unicode (UTF-8)", "")
    if (isTRUE(sp$short)) {
      idx <- if (isTRUE(sp$varnum) || identical(sp$order, "varnum")) seq_len(n) else contents_alpha_order(cv$disp, sp$order %||% "ascii")
      lst <- c(lst, if (isTRUE(sp$varnum) || identical(sp$order, "varnum")) paste0("Variables in Creation Order") else
        paste0("Alphabetic List of Variables for WORK.", toupper(name)), "", paste(cv$disp[idx], collapse = " "), "")
    } else {
      if (isTRUE(sp$varnum) || identical(sp$order, "varnum")) {
        lst <- c(lst, contents_var_table(cv, seq_len(n), "Variables in Creation Order"))
      } else {
        lst <- c(lst, contents_var_table(cv, contents_alpha_order(cv$disp, sp$order %||% "ascii"), "Alphabetic List of Variables and Attributes"))
        if (isTRUE(sp$position)) lst <- c(lst, contents_var_table(cv, seq_len(n), "Variables in Creation Order"))
      }
    }
    if (!is.null(m$sortedby)) {
      lst <- c(lst, "Sort Information", "", paste0("Sortedby       ", paste(ifelse(m$sortedby$descending, paste("DESCENDING", toupper(m$sortedby$vars)), m$sortedby$vars), collapse = " ")),
               "Validated      YES", "Character Set  ANSI", "")
    }
  }
  out <- list()
  if (!is.null(sp$out)) {
    ord <- order(cv$disp, method = "radix")
    fl <- function(f, part) {
      vapply(cv$vars[ord], function(v) {
        x <- f[[v]]
        if (is.null(x)) return(0)
        val <- if (part == "w") x$w else x$d
        if (is.na(val)) 0 else as.numeric(val)
      }, 1, USE.NAMES = FALSE)
    }
    fname <- function(f) vapply(cv$vars[ord], function(v) {
      x <- f[[v]]
      if (is.null(x)) "" else toupper(paste0(if (isTRUE(x$char)) "$", x$name))
    }, "", USE.NAMES = FALSE)
    crdate <- as.numeric(difftime(Sys.time(), as.POSIXct("1960-01-01", tz = "UTC"), units = "secs"))
    sb <- m$sortedby
    data <- list(
      libname = rep("WORK", n), memname = rep(toupper(name), n), memlabel = rep(m$memlabel %||% "", n),
      typemem = rep(m$typemem %||% "", n), name = cv$disp[ord], type = ifelse(cv$type[ord] == "num", 1, 2),
      length = as.numeric(cv$len[ord]), varnum = as.numeric(seq_len(n)[ord]), label = cv$label[ord],
      format = fname(m$formats), formatl = fl(m$formats, "w"), formatd = fl(m$formats, "d"),
      informat = fname(m$informats), informl = fl(m$informats, "w"), informd = fl(m$informats, "d"),
      just = ifelse(cv$type[ord] == "num", 1, 0), npos = as.numeric(np$npos[ord]), nobs = rep(nobs, n),
      engine = rep("V9", n), crdate = rep(crdate, n), modate = rep(crdate, n), delobs = rep(0, n),
      idxusage = rep("NONE", n), memtype = rep("DATA", n), idxcount = rep(0, n), protect = rep("---", n),
      flags = rep("---", n), compress = rep("NO", n), reuse = rep("NO", n),
      sorted = rep(if (is.null(sb)) NA_real_ else 1, n),
      sortedby = vapply(cv$vars[ord], function(v) if (is.null(sb) || !v %in% sb$vars) NA_real_ else as.numeric(match(v, sb$vars)), 1, USE.NAMES = FALSE),
      charset = rep("", n), collate = rep("", n), nodupkey = rep(if (isTRUE(sb$nodupkey)) "YES" else "NO", n),
      noduprec = rep(if (isTRUE(sb$noduprec)) "YES" else "NO", n), encrypt = rep("NO", n), pointobs = rep("YES", n),
      genmax = rep(0, n), gennum = rep(NA_real_, n), gennext = rep(NA_real_, n), transcod = rep("YES", n)
    )
    spec <- list(
      c("libname", "LIBNAME", 8, "Library Name"), c("memname", "MEMNAME", 32, "Library Member Name"),
      c("memlabel", "MEMLABEL", 256, "Data Set Label"), c("typemem", "TYPEMEM", 8, "Special Data Set Type (From TYPE=)"),
      c("name", "NAME", 32, "Variable Name"), c("type", "TYPE", NA, "Variable Type"), c("length", "LENGTH", NA, "Variable Length"),
      c("varnum", "VARNUM", NA, "Variable Number"), c("label", "LABEL", 256, "Variable Label"), c("format", "FORMAT", 32, "Variable Format"),
      c("formatl", "FORMATL", NA, "Format Length"), c("formatd", "FORMATD", NA, "Number of Format Decimals"),
      c("informat", "INFORMAT", 32, "Variable Informat"), c("informl", "INFORML", NA, "Informat Length"),
      c("informd", "INFORMD", NA, "Number of Informat Decimals"), c("just", "JUST", NA, "Justification"),
      c("npos", "NPOS", NA, "Position in Buffer"), c("nobs", "NOBS", NA, "Observations in Data Set"),
      c("engine", "ENGINE", 8, "Engine Name"), c("crdate", "CRDATE", NA, "Create Date"), c("modate", "MODATE", NA, "Last Modified Date"),
      c("delobs", "DELOBS", NA, "Deleted Observations in Data Set"), c("idxusage", "IDXUSAGE", 9, "Use of Variable in Indexes"),
      c("memtype", "MEMTYPE", 8, "Library Member Type"), c("idxcount", "IDXCOUNT", NA, "Number of Indexes for Data Set"),
      c("protect", "PROTECT", 3, "Password Protection (Read Write Alter)"), c("flags", "FLAGS", 3, "Update Flags (Protect Contribute Add)"),
      c("compress", "COMPRESS", 8, "Compression Routine"), c("reuse", "REUSE", 3, "Reuse Space"),
      c("sorted", "SORTED", NA, "Sorted and/or Validated"), c("sortedby", "SORTEDBY", NA, "Position of Variable in Sortedby Clause"),
      c("charset", "CHARSET", 8, "Host Character Set"), c("collate", "COLLATE", 8, "Collating Sequence"),
      c("nodupkey", "NODUPKEY", 3, "Sort Option: No Duplicate Keys"), c("noduprec", "NODUPREC", 3, "Sort Option: No Duplicate Records"),
      c("encrypt", "ENCRYPT", 8, "Encryption Routine"), c("pointobs", "POINTOBS", 3, "Point to Observations"),
      c("genmax", "GENMAX", NA, "Maximum Number of Generations"), c("gennum", "GENNUM", NA, "Generation Number"),
      c("gennext", "GENNEXT", NA, "Next Generation Number"), c("transcod", "TRANSCOD", 3, "Character Variables Transcoded")
    )
    cols <- lapply(spec, function(s) {
      char <- !is.na(s[3])
      list(name = s[1], disp = s[2], char = char, len = if (char) as.integer(s[3]) else 8L, label = s[4],
           fmt = if (s[1] %in% c("crdate", "modate")) list(name = "datetime", w = 16L, d = NA_integer_, char = FALSE, text = "DATETIME16.") else NULL)
    })
    names(data) <- vapply(spec, `[`, "", 1)
    out[[sp$out$name]] <- proc_out_options(tibble::as_tibble(data), proc_out_meta(cols), sp$out$options)
  }
  list(listing = lst, out = out)
}

translate_proc_contents <- function(step, meta, last) {
  o <- step$options
  ref <- o$data
  if (is.null(ref)) {
    if (is.null(last)) abort_runtime("There is no default input data set (_LAST_ is _NULL_)", step$line)
    ref <- list(name = last, options = list())
  }
  if (length(ref$options) > 0) abort_unsupported("data set options on PROC CONTENTS DATA=", step$line)
  m <- meta[[ref$name]]
  if (is.null(m)) abort_runtime(paste0("File WORK.", toupper(ref$name), ".DATA does not exist."), step$line)
  sp <- o[c("noprint", "varnum", "short", "position", "order", "out")]
  sp <- Filter(Negate(is.null), sp)
  code <- c(sprintf("# ---- PROC CONTENTS (SAS line %d)", as.integer(step$line)),
            sprintf(".r <- sas_proc_contents(work, %s, %s, %s)", r_str(ref$name), r_lit(m), r_lit(sp)),
            ".lst <- c(.lst, .titles_once(.titles, .r$listing))",
            ".meta <- c(.meta, sas_proc_store(work, .r$out))")
  list(code = paste(code, collapse = "\n"), level = "PROC", reasons = "PROC CONTENTS", line = step$line,
       outputs = if (!is.null(o$out)) o$out$name else character(), meta = list())
}

# PROC DATASETS ---------------------------------------------------------------------

parse_proc_datasets <- function(p, line) {
  advance(p)
  opts <- list()
  while (tk_type(p) != "semi") {
    if (at_eof(p)) abort_syntax("missing semicolon after PROC DATASETS", line)
    if (tk_type(p) != "name") abort_syntax(paste("unexpected", tok_desc(p), "in PROC DATASETS"), line)
    o <- tk_low(p)
    oline <- tk_line(p)
    if (is_op(p, "=", 1L)) {
      advance(p, 2L)
      v <- tolower(parse_opt_value(p, o, oline))
      if (o %in% c("library", "lib", "ddname", "dd")) {
        if (v != "work") abort_unsupported(paste0("library ", toupper(v), " (only WORK is available)"), oline)
      } else if (o %in% c("memtype", "mt", "mtype")) {
        if (v != "data") abort_unsupported(paste0("MEMTYPE=", toupper(v)), oline)
      } else {
        abort_unsupported(paste0("PROC DATASETS option ", toupper(o), "="), oline)
      }
      next
    }
    if (!o %in% c("nolist", "nodetails", "details", "kill", "noprint", "nowarn", "force")) abort_unsupported(paste0("PROC DATASETS option ", toupper(o)), oline)
    opts[[o]] <- TRUE
    advance(p)
  }
  advance(p)
  if (!isTRUE(opts$nolist) && !isTRUE(opts$noprint)) abort_unsupported("PROC DATASETS directory listing (use NOLIST)", line)
  body <- proc_parse_body(p, "datasets", function(p, kw, sline) {
    switch(kw,
      delete = {
        s <- parse_items_stmt(p, "delete")
        s
      },
      save = abort_unsupported("DATASETS SAVE", sline),
      change = {
        advance(p)
        pairs <- character()
        while (tk_type(p) == "name" && is_op(p, "=", 1L)) {
          pairs[tk_low(p)] <- tk_low(p, 2L)
          advance(p, 3L)
        }
        if (is_op(p, "/")) abort_unsupported("CHANGE statement options", sline)
        expect_semi(p, "CHANGE")
        list(stmt = "change", pairs = pairs, line = sline)
      },
      modify = {
        advance(p)
        ref <- parse_ds_ref_dslabel(p, sline)
        if (is_op(p, "/")) abort_unsupported("MODIFY statement options", sline)
        expect_semi(p, "MODIFY")
        list(stmt = "modify", name = ref$name, label = ref$label, line = sline)
      },
      format = parse_format_stmt(p),
      informat = {
        s <- parse_format_stmt(p)
        s$stmt <- "informat"
        s
      },
      label = parse_dd_label(p),
      rename = {
        advance(p)
        pairs <- parse_rename_pairs(p, paren = FALSE)
        expect_semi(p, "RENAME")
        list(stmt = "rename", pairs = pairs, line = sline)
      },
      attrib = parse_dd_attrib(p),
      length = {
        while (tk_type(p) != "semi") advance(p)
        advance(p)
        list(stmt = "length_ignored", line = sline)
      },
      contents = {
        advance(p)
        list(stmt = "contents", opts = parse_contents_opts(p, sline, "CONTENTS"), line = sline)
      },
      append = {
        advance(p)
        o <- list()
        while (tk_type(p) == "name") {
          k <- tk_low(p)
          if (k == "force") {
            o$force <- TRUE
            advance(p)
            next
          }
          if (!k %in% c("base", "data", "out", "new") || !is_op(p, "=", 1L)) abort_unsupported(paste0("APPEND option ", toupper(k)), sline)
          advance(p, 2L)
          r <- parse_ds_ref(p, if (k == "data") "input" else "output")
          o[[if (k %in% c("out")) "base" else if (k == "new") "data" else k]] <- r
        }
        if (is_op(p, "/")) {
          advance(p)
          while (tk_type(p) == "name") {
            if (tk_low(p) != "force") abort_unsupported(paste0("APPEND option ", toupper(tk_val(p))), sline)
            o$force <- TRUE
            advance(p)
          }
        }
        expect_semi(p, "APPEND")
        list(stmt = "append", base = o$base, data = o$data, force = isTRUE(o$force), line = sline)
      },
      page = {
        advance(p)
        expect_semi(p, "PAGE")
        NULL
      },
      footnote = ,
      footnote1 = ,
      options = parse_global_stmt(p),
      abort_unsupported(paste0("PROC DATASETS statement ", toupper(kw)), sline)
    )
  }, run_ends = FALSE)
  list(type = "proc", name = "datasets", line = line, options = opts, stmts = body$stmts, titles = body$titles)
}

parse_ds_ref_dslabel <- function(p, line) {
  if (tk_type(p) != "name") abort_syntax("expected a data set name", line)
  name <- tk_low(p)
  advance(p)
  if (tk_type(p) == "dot" && tk_type(p, 1L) == "name") {
    if (name != "work") abort_unsupported(paste0("library ", toupper(name), " (only WORK is available)"), line)
    name <- tk_low(p, 1L)
    advance(p, 2L)
  }
  label <- NULL
  if (tk_type(p) == "lparen") {
    advance(p)
    while (tk_type(p) != "rparen") {
      k <- tk_low(p)
      if (k != "label" || !is_op(p, "=", 1L)) abort_unsupported(paste0("MODIFY data set option ", toupper(tk_val(p))), line)
      advance(p, 2L)
      if (!tk_type(p) %in% c("str", "str_macro")) abort_syntax("expected a quoted label", line)
      label <- tk_val(p)
      advance(p)
    }
    advance(p)
  }
  list(name = name, label = label)
}

parse_dd_label <- function(p) {
  line <- tk_line(p)
  advance(p)
  labels <- list()
  while (tk_type(p) == "name" && is_op(p, "=", 1L)) {
    nm <- tk_low(p)
    advance(p, 2L)
    if (tk_type(p) %in% c("str", "str_macro")) {
      labels[[nm]] <- tk_val(p)
      advance(p)
    } else {
      labels[nm] <- list(NULL)
      labels[[nm]] <- ""
    }
  }
  expect_semi(p, "LABEL")
  list(stmt = "label", labels = labels, line = line)
}

# ATTRIB inside MODIFY: FORMAT= / INFORMAT= / LABEL= (empty values remove).
parse_dd_attrib <- function(p) {
  line <- tk_line(p)
  advance(p)
  groups <- list()
  while (tk_type(p) != "semi") {
    vars <- list()
    while (tk_type(p) == "name" && !is_op(p, "=", 1L)) vars <- c(vars, list(parse_varlist_item(p)))
    if (length(vars) == 0) abort_syntax(paste("expected a variable in ATTRIB but found", tok_desc(p)), line)
    g <- list(vars = vars, set = list())
    while (tk_type(p) == "name" && is_op(p, "=", 1L)) {
      o <- tk_low(p)
      advance(p, 2L)
      empty <- tk_type(p) == "semi" || (tk_type(p) == "name" && (is_op(p, "=", 1L) || !is_format_ahead(p)))
      if (o %in% c("format", "informat")) {
        g$set[[o]] <- if (empty) list(remove = TRUE) else parse_format(p)
      } else if (o == "label") {
        if (tk_type(p) %in% c("str", "str_macro")) {
          g$set$label <- tk_val(p)
          advance(p)
        } else {
          g$set$label <- ""
        }
      } else {
        abort_unsupported(paste0("ATTRIB option ", toupper(o), "= in PROC DATASETS"), line)
      }
    }
    groups <- c(groups, list(g))
  }
  advance(p)
  list(stmt = "attrib", groups = groups, line = line)
}

# PROC DATASETS stores a format written as `w.d` under the name F.
dd_named_fmt <- function(f) {
  if (is.null(f) || nzchar(f$name) || isTRUE(f$char)) return(f)
  f$name <- "f"
  f$text <- paste0("F", f$text)
  f
}

# PROC DATASETS runs on the engine directly (it manages WORK members and their
# descriptors).
engine_datasets <- function(eng, step) {
  notes <- character()
  note <- function(x) notes <<- c(notes, x)
  exists_ds <- function(nm) exists(nm, envir = eng$wenv, inherits = FALSE)
  drop_ds <- function(nm) {
    rm(list = nm, envir = eng$wenv)
    eng$meta[[nm]] <- NULL
  }
  if (isTRUE(step$options$kill)) {
    for (nm in sort(ls(eng$wenv))) {
      note(sprintf("NOTE: Deleting WORK.%s (memtype=DATA).", toupper(nm)))
      drop_ds(nm)
    }
    rt$catalog <- new.env(parent = emptyenv())
  }
  cur <- NULL
  modified <- NULL
  end_modify <- function() {
    if (!is.null(modified)) note(sprintf("NOTE: MODIFY was successful for WORK.%s.DATA.", toupper(modified)))
    modified <<- NULL
  }
  resolve_vars <- function(items, m, line) {
    st <- new_state(list())
    st$order <- m$vars
    st$info <- lapply(stats::setNames(m$vars, m$vars), function(v) list(type = m$type[[v]], kind = "data"))
    vs <- resolve_items(items, st, line, register = FALSE)
    miss <- setdiff(vs, m$vars)
    if (length(miss) > 0) abort_runtime(paste0("Variable ", toupper(miss[1]), " not found."), line)
    vs
  }
  lst <- character()
  for (s in step$stmts) {
    if (identical(s$type, "global")) {
      engine_global(eng, s)
      next
    }
    if (!s$stmt %in% c("format", "informat", "label", "rename", "attrib", "length_ignored")) end_modify()
    switch(s$stmt,
      run = NULL,
      delete = {
        members <- ls(eng$wenv)
        for (it in s$items) {
          nms <- switch(it$kind,
            name = it$name,
            prefix = members[startsWith(members, it$prefix)],
            range = expand_numbered_range(it$from, it$to, s$line),
            special = if (it$name == "_all_") members else abort_unsupported("DELETE list form", s$line),
            abort_unsupported("DELETE list form", s$line))
          for (nm in nms) {
            if (exists_ds(nm)) {
              note(sprintf("NOTE: Deleting WORK.%s (memtype=DATA).", toupper(nm)))
              drop_ds(nm)
            } else if (it$kind == "name") {
              note(sprintf("WARNING: The file WORK.%s (memtype=DATA) was not found, but appears on a DELETE statement.", toupper(nm)))
            }
          }
        }
      },
      change = {
        for (old in names(s$pairs)) {
          new <- s$pairs[[old]]
          if (!exists_ds(old)) abort_runtime(sprintf("The file WORK.%s (memtype=DATA) was not found, but appears on a CHANGE statement.", toupper(old)), s$line)
          if (exists_ds(new)) abort_runtime(sprintf("Cannot rename WORK.%s to WORK.%s because WORK.%s already exists.", toupper(old), toupper(new), toupper(new)), s$line)
          assign(new, get(old, envir = eng$wenv), envir = eng$wenv)
          eng$meta[[new]] <- eng$meta[[old]]
          drop_ds(old)
          note(sprintf("NOTE: Changing the name WORK.%s to WORK.%s (memtype=DATA).", toupper(old), toupper(new)))
        }
      },
      modify = {
        if (!exists_ds(s$name)) abort_runtime(sprintf("File WORK.%s.DATA does not exist.", toupper(s$name)), s$line)
        cur <- s$name
        modified <- s$name
        if (!is.null(s$label)) eng$meta[[cur]]$memlabel <- s$label
        eng$meta[[cur]]$sortedby <- eng$meta[[cur]]$sortedby
      },
      length_ignored = note("NOTE: The LENGTH attribute cannot be changed and is therefore being ignored."),
      format = ,
      informat = {
        if (is.null(cur)) abort_syntax(paste(toupper(s$stmt), "statement without a preceding MODIFY"), s$line)
        slot <- if (s$stmt == "format") "formats" else "informats"
        m <- eng$meta[[cur]]
        for (sp in s$specs) {
          for (v in resolve_vars(sp$vars, m, s$line)) m[[slot]][[v]] <- dd_named_fmt(sp$fmt)
        }
        eng$meta[[cur]] <- m
      },
      label = {
        if (is.null(cur)) abort_syntax("LABEL statement without a preceding MODIFY", s$line)
        m <- eng$meta[[cur]]
        for (v in names(s$labels)) {
          if (!v %in% m$vars) abort_runtime(paste0("Variable ", toupper(v), " not found."), s$line)
          m$labels[[v]] <- if (nzchar(s$labels[[v]])) s$labels[[v]] else NULL
        }
        eng$meta[[cur]] <- m
      },
      attrib = {
        if (is.null(cur)) abort_syntax("ATTRIB statement without a preceding MODIFY", s$line)
        m <- eng$meta[[cur]]
        for (g in s$groups) {
          for (v in resolve_vars(g$vars, m, s$line)) {
            for (k in names(g$set)) {
              val <- g$set[[k]]
              if (k == "label") {
                m$labels[[v]] <- if (nzchar(val)) val else NULL
              } else {
                slot <- if (k == "format") "formats" else "informats"
                m[[slot]][[v]] <- if (isTRUE(val$remove)) NULL else dd_named_fmt(val)
              }
            }
          }
        }
        eng$meta[[cur]] <- m
      },
      rename = {
        if (is.null(cur)) abort_syntax("RENAME statement without a preceding MODIFY", s$line)
        m <- eng$meta[[cur]]
        d <- get(cur, envir = eng$wenv)
        for (old in names(s$pairs)) {
          new <- s$pairs[[old]]
          if (!old %in% m$vars) abort_runtime(paste0("Variable ", toupper(old), " is not on file WORK.", toupper(cur), "."), s$line)
          if (new %in% m$vars) abort_runtime(paste0("Variable ", toupper(new), " already exists on file WORK.", toupper(cur), "."), s$line)
          ren <- function(l) {
            if (length(l) && old %in% names(l)) names(l)[names(l) == old] <- new
            l
          }
          m$vars[m$vars == old] <- new
          for (sl in c("type", "len", "formats", "informats", "labels", "disp")) m[[sl]] <- ren(m[[sl]])
          m$disp[[new]] <- new
          names(d)[names(d) == old] <- new
          note(sprintf("NOTE: Renaming variable %s to %s.", old, new))
        }
        assign(cur, d, envir = eng$wenv)
        eng$meta[[cur]] <- m
      },
      contents = {
        o <- s$opts
        ref <- o$data %||% (if (!is.null(cur)) list(name = cur, options = list())) %||% list(name = eng$last, options = list())
        m <- eng$meta[[ref$name]]
        if (is.null(m)) abort_runtime(paste0("File WORK.", toupper(ref$name), ".DATA does not exist."), s$line)
        sp <- Filter(Negate(is.null), o[c("noprint", "varnum", "short", "position", "order", "out")])
        sp$proc <- "DATASETS"
        r <- sas_proc_contents(eng$wenv, ref$name, m, sp)
        if (length(r$listing) > 0) lst <- c(lst, if (length(lst) == 0) eng$titles, if (length(lst) == 0 && length(eng$titles)) "", r$listing)
        eng$meta[names(r$out)] <- sas_proc_store(eng$wenv, r$out)
        for (nm in names(r$out)) notes <- c(notes, dataset_note(nm, get(nm, envir = eng$wenv)))
      },
      append = {
        r <- datasets_append(eng, s)
        notes <- c(notes, r)
      },
      abort_unsupported(paste("PROC DATASETS statement", toupper(s$stmt)), s$line)
    )
  }
  end_modify()
  if (isTRUE(rt$notes_on) && eng$step_notes) eng$log <- c(eng$log, notes)
  eng$listing <- c(eng$listing, lst)
  invisible()
}

# APPEND BASE= DATA= (also PROC APPEND): the base keeps its variables and
# attributes; variables only in DATA= are dropped (FORCE), character values
# are truncated to the base length.
datasets_append <- function(eng, s) {
  if (is.null(s$base) || is.null(s$data)) abort_syntax("APPEND requires BASE= and DATA=", s$line)
  bn <- s$base$name
  dn <- s$data$name
  if (length(s$base$options) > 0 || length(s$data$options) > 0) abort_unsupported("data set options in APPEND", s$line)
  if (!exists(dn, envir = eng$wenv, inherits = FALSE)) abort_runtime(sprintf("File WORK.%s.DATA does not exist.", toupper(dn)), s$line)
  dd <- get(dn, envir = eng$wenv)
  dm <- eng$meta[[dn]]
  notes <- sprintf("NOTE: Appending WORK.%s to WORK.%s.", toupper(dn), toupper(bn))
  if (!exists(bn, envir = eng$wenv, inherits = FALSE)) {
    assign(bn, dd, envir = eng$wenv)
    eng$meta[[bn]] <- dm[setdiff(names(dm), c("sortedby", "memlabel"))]
    return(c(notes, sprintf("NOTE: BASE data set does not exist. DATA file is being copied to BASE file."),
             sprintf("NOTE: There were %d observations read from the data set WORK.%s.", nrow(dd), toupper(dn)),
             dataset_note(bn, dd)))
  }
  bd <- get(bn, envir = eng$wenv)
  bm <- eng$meta[[bn]]
  extra <- setdiff(dm$vars, bm$vars)
  missing_in_data <- setdiff(bm$vars, dm$vars)
  for (v in intersect(bm$vars, dm$vars)) {
    if (!identical(bm$type[[v]], dm$type[[v]])) {
      if (!s$force) abort_runtime(sprintf("Variable %s not appended because of type mismatch.", v), s$line)
      abort_unsupported("APPEND FORCE with a type mismatch", s$line)
    }
  }
  longer <- Filter(function(v) identical(bm$type[[v]], "char") && dm$len[[v]] > bm$len[[v]], intersect(bm$vars, dm$vars))
  if ((length(extra) > 0 || length(longer) > 0) && !s$force) {
    abort_runtime("No appending done because of anomalies listed above. Use FORCE option to append these files.", s$line)
  }
  add <- dd[intersect(dm$vars, bm$vars)]
  for (v in missing_in_data) add[[v]] <- if (identical(bm$type[[v]], "char")) "" else rep(NA_real_, nrow(add))
  add <- add[bm$vars]
  for (v in bm$vars) if (identical(bm$type[[v]], "char")) add[[v]] <- sas_char(add[[v]], bm$len[[v]])
  out <- dplyr::bind_rows(bd, add)
  assign(bn, tibble::as_tibble(out), envir = eng$wenv)
  bm$sortedby <- NULL
  eng$meta[[bn]] <- bm
  c(notes, sprintf("NOTE: There were %d observations read from the data set WORK.%s.", nrow(dd), toupper(dn)),
    sprintf("NOTE: %d observations added.", nrow(dd)),
    sprintf("NOTE: The data set WORK.%s has %d observations and %d variables.", toupper(bn), nrow(out), ncol(out)))
}

# PROC APPEND BASE= DATA= [FORCE]; and PROC DELETE DATA= ...; share the
# DATASETS engine.
parse_proc_append <- function(p, line) {
  advance(p)
  o <- list()
  while (tk_type(p) == "name") {
    k <- tk_low(p)
    if (k %in% c("force", "nowarn")) {
      o[[k]] <- TRUE
      advance(p)
      next
    }
    if (!k %in% c("base", "data", "out", "new") || !is_op(p, "=", 1L)) abort_unsupported(paste0("PROC APPEND option ", toupper(k)), line)
    advance(p, 2L)
    r <- parse_ds_ref(p, if (k %in% c("data", "new")) "input" else "output")
    o[[if (k == "out") "base" else if (k == "new") "data" else k]] <- r
  }
  expect_semi(p, "PROC APPEND")
  body <- proc_parse_body(p, "append", function(p, kw, sline) abort_syntax(paste0("Statement ", toupper(kw), " is not valid in PROC APPEND"), sline))
  if (is.null(o$data)) abort_unsupported("PROC APPEND without DATA= (_LAST_)", line)
  list(type = "proc", name = "datasets", line = line, options = list(nolist = TRUE),
       stmts = list(list(stmt = "append", base = o$base, data = o$data, force = isTRUE(o$force), line = line)), titles = body$titles)
}

parse_proc_delete <- function(p, line) {
  advance(p)
  items <- list()
  while (tk_type(p) == "name") {
    k <- tk_low(p)
    if (k == "data" && is_op(p, "=", 1L)) {
      advance(p, 2L)
      while (tk_type(p) == "name" && !is_op(p, "=", 1L)) {
        r <- parse_ds_ref(p, "input")
        items <- c(items, list(list(kind = "name", name = r$name)))
      }
      next
    }
    abort_unsupported(paste0("PROC DELETE option ", toupper(k)), line)
  }
  expect_semi(p, "PROC DELETE")
  body <- proc_parse_body(p, "delete", function(p, kw, sline) abort_syntax(paste0("Statement ", toupper(kw), " is not valid in PROC DELETE"), sline))
  list(type = "proc", name = "datasets", line = line, options = list(nolist = TRUE),
       stmts = list(list(stmt = "delete", items = items, line = line)), titles = body$titles)
}
