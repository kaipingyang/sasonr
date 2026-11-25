# DICTIONARY tables of PROC SQL (spec 03 P0-3): COLUMNS and TABLES, built
# from the session's metadata of the WORK members (data sets and SQL views).
#
# Measured: the columns, their lengths and labels; npos counts the
# numeric columns first, then the character ones, in creation order; type and
# xtype are "char" / "num"; notnull "no", precision 0, scale missing,
# transcode "yes", idxusage blank; sortedby is the position in the sort key
# (0 when not sorted by it).  A view has a missing nobs.
# The dictionary lists every library (SASHELP too): sasonr builds the WORK
# rows only, so a query must restrict libname to 'WORK' (else it fails loud).
# Columns that depend on the host (dates, sizes, pages, encoding...) fail
# loud when a query names them (also through `*`).

sql_dict_cols <- list(
  columns = list(
    c("libname", "char", 8L, "Library Name"), c("memname", "char", 32L, "Member Name"),
    c("memtype", "char", 8L, "Member Type"), c("name", "char", 32L, "Column Name"),
    c("type", "char", 4L, "Column Type"), c("length", "num", 8L, "Column Length"),
    c("npos", "num", 8L, "Column Position"), c("varnum", "num", 8L, "Column Number in Table"),
    c("label", "char", 256L, "Column Label"), c("format", "char", 49L, "Column Format"),
    c("informat", "char", 49L, "Column Informat"), c("idxusage", "char", 9L, "Column Index Type"),
    c("sortedby", "num", 8L, "Order in Key Sequence"), c("xtype", "char", 12L, "Extended Type"),
    c("notnull", "char", 3L, "Not NULL?"), c("precision", "num", 8L, "Precision"),
    c("scale", "num", 8L, "Scale"), c("transcode", "char", 3L, "Transcoded?")
  ),
  tables = list(
    c("libname", "char", 8L, "Library Name"), c("memname", "char", 32L, "Member Name"),
    c("memtype", "char", 8L, "Member Type"), c("dbms_memtype", "char", 32L, "DBMS Member Type"),
    c("memlabel", "char", 256L, "Data Set Label"), c("typemem", "char", 8L, "Data Set Type"),
    c("crdate", "num", 8L, "Date Created"), c("modate", "num", 8L, "Date Modified"),
    c("nobs", "num", 8L, "Number of Physical Observations"), c("obslen", "num", 8L, "Observation Length"),
    c("nvar", "num", 8L, "Number of Variables"), c("protect", "char", 3L, "Type of Password Protection"),
    c("compress", "char", 8L, "Compression Routine"), c("encrypt", "char", 8L, "Encryption"),
    c("npage", "num", 8L, "Number of Pages"), c("filesize", "num", 8L, "Size of File"),
    c("pcompress", "num", 8L, "Percent Compression"), c("reuse", "char", 3L, "Reuse Space"),
    c("bufsize", "num", 8L, "Bufsize"), c("delobs", "num", 8L, "Number of Deleted Observations"),
    c("nlobs", "num", 8L, "Number of Logical Observations"), c("maxvar", "num", 8L, "Longest variable name"),
    c("maxlabel", "num", 8L, "Longest label"), c("maxgen", "num", 8L, "Maximum number of generations"),
    c("gen", "num", 8L, "Generation number"), c("attr", "char", 3L, "Data Set Attributes"),
    c("indxtype", "char", 9L, "Type of Indexes"), c("datarep", "char", 32L, "Data Representation"),
    c("sortname", "char", 8L, "Name of Collating Sequence"), c("sorttype", "char", 4L, "Sorting Type"),
    c("sortchar", "char", 8L, "Charset Sorted By"), c("reqvector", "char", 24L, "Requirements Vector"),
    c("datarepname", "char", 170L, "Data Representation Name"), c("encoding", "char", 256L, "Data Encoding"),
    c("audit", "char", 3L, "Audit Trail Active?"), c("audit_before", "char", 3L, "Audit Before Image?"),
    c("audit_admin", "char", 3L, "Audit Admin Image?"), c("audit_error", "char", 3L, "Audit Error Image?"),
    c("audit_data", "char", 3L, "Audit Data Image?"), c("num_character", "num", 8L, "Number of Character Variables"),
    c("num_numeric", "num", 8L, "Number of Numeric Variables")
  )
)

# the columns sasonr computes (the others fail loud when named)
sql_dict_known <- list(
  columns = vapply(sql_dict_cols$columns, `[`, "", 1L),
  tables = c("libname", "memname", "memtype", "memlabel", "typemem", "nobs", "nlobs", "nvar", "num_character",
             "num_numeric", "delobs")
)

# SASHELP views of the dictionary tables
sql_dict_views <- c(vcolumn = "columns", vtable = "tables")

sql_dict_name <- function(libref, name) {
  if (identical(libref, "dictionary")) return(name)
  if (identical(libref, "sashelp") && name %in% names(sql_dict_views)) return(sql_dict_views[[name]])
  NULL
}

# The WORK members: list(name, type DATA/VIEW, meta, nobs).
sql_dict_members <- function(eng) {
  s <- eng$session
  out <- list()
  for (nm in ls(s$work)) {
    d <- get(nm, envir = s$work)
    out[[nm]] <- list(name = nm, type = "DATA", meta = s$meta[[nm]] %||% meta_from_df(d), nobs = nrow(d))
  }
  for (nm in names(s$views)) {
    res <- sql_query(s$views[[nm]], eng)
    tb <- sql_rel_to_table(eng, res, NA_integer_)
    out[[nm]] <- list(name = nm, type = "VIEW", meta = tb$meta, nobs = NA_real_)
  }
  out[order(toupper(names(out)), method = "radix")]
}

sql_dict_fmt <- function(f) if (is.null(f)) "" else toupper(f$text %||% "")

sql_dict_data <- function(eng, kind, line) {
  if (!kind %in% names(sql_dict_cols)) abort_unsupported(paste0("DICTIONARY.", toupper(kind)), line)
  mem <- sql_dict_members(eng)
  rows <- list()
  for (m in mem) {
    me <- m$meta
    vars <- me$vars
    type <- vapply(vars, function(v) me$type[[v]] %||% "num", "")
    len <- vapply(vars, function(v) as.numeric(me$len[[v]] %||% 8L), 1)
    if (kind == "columns") {
      num <- type == "num"
      npos <- numeric(length(vars))
      npos[num] <- c(0, cumsum(len[num]))[seq_len(sum(num))]
      off <- sum(len[num])
      npos[!num] <- off + c(0, cumsum(len[!num]))[seq_len(sum(!num))]
      sb <- me$sortedby$vars
      for (j in seq_along(vars)) {
        v <- vars[j]
        rows[[length(rows) + 1L]] <- list(
          libname = "WORK", memname = toupper(m$name), memtype = m$type, name = me$disp[[v]] %||% v,
          type = type[[j]], length = len[[j]], npos = npos[j], varnum = j, label = me$labels[[v]] %||% "",
          format = sql_dict_fmt(me$formats[[v]]), informat = sql_dict_fmt(me$informats[[v]]), idxusage = "",
          sortedby = if (v %in% sb) match(v, sb) else 0, xtype = type[[j]], notnull = "no", precision = 0,
          scale = NA_real_, transcode = "yes")
      }
    } else {
      rows[[length(rows) + 1L]] <- list(
        libname = "WORK", memname = toupper(m$name), memtype = m$type, memlabel = me$memlabel %||% "",
        typemem = me$typemem %||% "", nobs = m$nobs, nlobs = m$nobs, nvar = length(vars),
        num_character = sum(type == "char"), num_numeric = sum(type == "num"), delobs = if (m$type == "VIEW") NA_real_ else 0)
    }
  }
  spec <- sql_dict_cols[[kind]]
  cols <- lapply(spec, function(c) {
    v <- lapply(rows, function(r) r[[c[1]]] %||% (if (c[2] == "char") "" else NA_real_))
    if (c[2] == "char") vapply(v, as.character, "") else vapply(v, as.numeric, 1)
  })
  names(cols) <- vapply(spec, `[`, "", 1L)
  d <- tibble::as_tibble(cols, .rows = length(rows))
  m <- proc_out_meta(lapply(spec, function(c) list(name = c[1], disp = c[1], char = c[2] == "char", len = as.integer(c[3]),
                                                   label = c[4])))
  list(data = d, meta = m)
}

# A query that reads a dictionary table: its WHERE must have a conjunct
# libname = 'WORK', and it may not name the host columns (nor `*`).
sql_dict_check <- function(q, kind, line) {
  known <- sql_dict_known[[kind]]
  walk <- function(x) {
    if (!is.list(x)) return(character())
    out <- character()
    if (identical(x$node, "col") && is.character(x$name)) out <- tolower(x$name)
    if (isTRUE(x$star)) out <- "*"
    for (el in x) out <- c(out, walk(el))
    out
  }
  used <- unique(c(walk(q$items), walk(q$where), walk(q$order), walk(q$group), walk(q$having)))
  bad <- setdiff(used, c(known, "*"))
  host <- intersect(bad, vapply(sql_dict_cols[[kind]], `[`, "", 1L))
  if ("*" %in% used && kind == "tables") host <- c(host, "*")
  if (length(host)) abort_unsupported(paste0("DICTIONARY.", toupper(kind), " column ", toupper(host[1]), " (its value depends on the host)"), line)
  conj <- function(e) if (is.list(e) && identical(e$node, "binary") && identical(tolower(e$op %||% ""), "and")) c(conj(e$lhs), conj(e$rhs)) else list(e)
  is_work <- function(e) {
    if (!is.list(e) || !identical(e$node, "compare") || !identical(e$op, "=")) return(FALSE)
    sides <- list(e$lhs, e$rhs)
    col <- vapply(sides, function(s) identical(s$node, "col") && identical(tolower(s$name), "libname"), logical(1))
    str <- vapply(sides, function(s) identical(s$node, "str") && identical(toupper(trimws(s$value %||% "")), "WORK"), logical(1))
    any(col) && any(str)
  }
  if (is.null(q$where) || !any(vapply(conj(q$where), is_work, logical(1)))) {
    abort_unsupported(paste0("DICTIONARY.", toupper(kind), " without WHERE libname = 'WORK' (sasonr lists the WORK members only)"), line)
  }
  invisible()
}
