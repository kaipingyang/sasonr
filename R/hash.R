# DATA step hash objects (spec 01 §4.4): DECLARE HASH and its methods in a
# row-by-row step. A hash object is an environment: the key and data
# variables, the entries (key code -> list of the key and data values, in
# insertion order) and the return code of the last method call.
#
# Return codes [measured]: 0 success; 160038 a key that is not in the hash
# object (FIND, CHECK); 160036 ADD of a key that is there already; 160032
# REMOVE of a key that is not there. REPLACE of a key that is not there adds
# it (0). A DECLARE that runs again makes a new, empty object. What is not
# measured fails loud: a failing method call used as a statement (an ERROR
# whose effect on the step is not measured), OUTPUT and iterators, SUMINC:
# and the methods not listed here.

hash_rc_missing_key <- 160038
hash_rc_duplicate <- 160036
hash_rc_remove_missing <- 160032

#' Create a hash object (DECLARE HASH)
#'
#' `...`: the constructor arguments by tag (`dataset`, `ordered`, `hashexp`);
#' `.work`: WORK (for DATASET:); `.lengths`: the lengths of the character
#' variables of that data set; `.line`: the program line.
#' @export
#' @keywords internal
sas_rt_hash_new <- function(..., .work = NULL, .lengths = NULL, .line = NULL) {
  args <- list(...)
  line <- .line
  known <- c("dataset", "ordered", "hashexp")
  bad <- setdiff(names(args), known)
  if (length(bad)) abort_unsupported(paste0("the DECLARE HASH argument ", toupper(bad[1]), ":"), line)
  h <- new.env(parent = emptyenv())
  h$keys <- character()
  h$data <- character()
  h$done <- FALSE
  h$entries <- list()
  h$rc <- 0
  h$line <- line
  ord <- tolower(trimws(args$ordered %||% "no"))
  h$ordered <- switch(ord, a = , ascending = , y = , yes = "a", d = , descending = "d", n = , no = "no",
                      abort_unsupported(paste0("ORDERED: '", args$ordered, "'"), line))
  h$dataset <- args$dataset
  h$work <- .work
  h$lengths <- .lengths
  h
}

hash_key_code <- function(vals) {
  paste(vapply(vals, function(v) {
    if (is.character(v)) return(paste0("c", sub(" +$", "", v)))
    if (is.na(v)) return(paste0("m", sas_miss_code(v)))
    if (v == 0) v <- 0
    paste0("n", sprintf("%a", v))
  }, ""), collapse = "\x1f")
}

hash_names <- function(args, what, line) {
  if (any(!vapply(args, function(a) is.null(a$tag), NA))) abort_unsupported(paste0("tagged arguments of ", toupper(what)), line)
  v <- vapply(args, function(a) if (is.character(a$value) && length(a$value) == 1L) tolower(trimws(a$value)) else NA_character_, "")
  if (any(v %in% c("_all_"))) abort_unsupported(paste0("ALL: of ", toupper(what)), line)
  if (anyNA(v)) abort_unsupported(paste0("arguments of ", toupper(what), " other than variable names"), line)
  v
}

#' Call a method of a hash object: the PDV after the call (the return code
#' in `h$rc`, sas_rt_hash_rc())
#'
#' `...`: the arguments, named by their tags (`key = `); `.stmt`: the call is
#' a statement (a failing call is an ERROR there).
#' @export
#' @keywords internal
sas_rt_hash_call <- function(h, method, pdv, ..., .stmt = FALSE) {
  args <- list(...)
  stmt <- .stmt
  line <- h$line
  if (identical(h$kind, "hiter")) {
    pdv <- hiter_call(h, method, pdv, args)
    if (stmt && h$rc != 0) abort_unsupported(paste0("a ", toupper(method), " method call statement that fails (an ERROR)"), line)
    return(pdv)
  }
  # the arguments as `list(tag, value)`
  tags <- names(args) %||% rep("", length(args))
  args <- lapply(seq_along(args), function(k) list(tag = if (nzchar(tags[k])) tags[k], value = args[[k]]))
  h$rc <- 0
  defined <- function() if (!h$done) abort_unsupported(paste0("the ", toupper(method), " method before DEFINEDONE"), line)
  key_vals <- function() {
    tagged <- Filter(function(a) identical(a$tag, "key"), args)
    if (length(args) && length(tagged) != length(args)) abort_unsupported(paste0("arguments of the ", toupper(method), " method"), line)
    if (length(tagged)) {
      if (length(tagged) != length(h$keys)) abort_unsupported(paste0("KEY: arguments of the ", toupper(method), " method"), line)
      return(lapply(tagged, `[[`, "value"))
    }
    pdv[h$keys]
  }
  switch(method,
    definekey = {
      h$keys <- c(h$keys, hash_names(args, method, line))
    },
    definedata = {
      if (length(args) == 1L && identical(args[[1]]$tag, "all")) {
        # ALL: 'YES': the variables of the DATASET: data set, in its order [measured]
        if (!tolower(trimws(args[[1]]$value)) %in% c("y", "yes") || is.null(h$dataset)) abort_unsupported("DEFINEDATA(ALL:) without DATASET: or other than 'YES'", line)
        h$data <- c(h$data, tolower(names(h$work[[tolower(trimws(h$dataset))]])))
      } else {
        h$data <- c(h$data, hash_names(args, method, line))
      }
    },
    definedone = {
      if (!length(h$keys)) abort_unsupported("DEFINEDONE without DEFINEKEY", line)
      miss <- setdiff(c(h$keys, h$data), names(pdv))
      if (length(miss)) abort_unsupported(paste0("the hash variable ", toupper(miss[1]), " that is not in the PDV"), line)
      # without DEFINEDATA the keys are the data
      if (!length(h$data)) h$data <- h$keys
      h$done <- TRUE
      if (!is.null(h$dataset)) hash_load(h, pdv, line)
    },
    add = , replace = {
      defined()
      # KEY: and DATA: give the values in the order of DEFINEKEY and
      # DEFINEDATA instead of the PDV's (all of them)
      kv <- pdv[h$keys]
      dv <- pdv[h$data]
      if (length(args)) {
        tags <- vapply(args, function(a) a$tag %||% "", "")
        if (!all(tags %in% c("key", "data")) || sum(tags == "key") != length(h$keys) ||
            sum(tags == "data") != length(h$data)) {
          abort_unsupported(paste0("arguments of the ", toupper(method), " method other than all KEY: and DATA: values"), line)
        }
        kv[] <- lapply(args[tags == "key"], `[[`, "value")
        dv[] <- lapply(args[tags == "data"], `[[`, "value")
        types_ok <- vapply(c(h$keys, h$data), function(v) is.character(c(kv, dv)[[v]]) == is.character(pdv[[v]]), NA)
        if (!all(types_ok)) abort_unsupported(paste0("a KEY: or DATA: value of another type than its variable (", toupper(method), ")"), line)
      }
      code <- hash_key_code(kv)
      have <- !is.null(h$entries[[code]])
      if (method == "add" && have) {
        h$rc <- hash_rc_duplicate
      } else {
        # character values take the length of their variable
        for (v in names(dv)) if (is.character(dv[[v]])) dv[[v]] <- sas_rt_char(dv[[v]], nchar(pdv[[v]], "bytes"))
        for (v in names(kv)) if (is.character(kv[[v]])) kv[[v]] <- sas_rt_char(kv[[v]], nchar(pdv[[v]], "bytes"))
        h$entries[[code]] <- list(key = kv, data = dv)
      }
    },
    find = , check = {
      defined()
      code <- hash_key_code(key_vals())
      e <- h$entries[[code]]
      if (is.null(e)) {
        h$rc <- hash_rc_missing_key
      } else if (method == "find") {
        pdv[h$data] <- e$data[h$data]
      }
    },
    output = {
      defined()
      if (length(args) != 1L || !identical(args[[1]]$tag, "dataset")) abort_unsupported("OUTPUT without one DATASET: argument", line)
      ds <- tolower(trimws(args[[1]]$value))
      if (h$ordered == "no") abort_unsupported("OUTPUT of a hash object without ORDERED: (the internal order of its entries)", line)
      ents <- unname(h$entries)
      keys <- if (length(ents)) do.call(rbind, lapply(ents, function(e) as.data.frame(e$key, stringsAsFactors = FALSE))) else NULL
      ord <- if (length(ents)) sas_order(keys, names(keys), rep(h$ordered == "d", ncol(keys))) else integer()
      h$written[[ds]] <- list(rows = lapply(ents[ord], `[[`, "data"), proto = pdv[h$data])
    },
    remove = {
      defined()
      code <- hash_key_code(key_vals())
      # an iterator on an entry locks the object: an ERROR [measured]
      if (any(vapply(h$iters, function(it) !is.null(it$cur), NA))) abort_unsupported("REMOVE from a hash object that an iterator has locked (an ERROR)", line)
      if (is.null(h$entries[[code]])) h$rc <- hash_rc_remove_missing else h$entries[[code]] <- NULL
    },
    abort_unsupported(paste0("the hash object method ", toupper(method)), line)
  )
  if (stmt && h$rc != 0) abort_unsupported(paste0("a ", toupper(method), " method call statement that fails (an ERROR)"), line)
  pdv
}

# DATASET: the observations of a WORK data set as entries (the first of
# several with one key); the values of a character variable take the length
# it has in the PDV (no WARNING [measured]).
hash_load <- function(h, pdv, line) {
  ds <- trimws(h$dataset)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", ds)) abort_unsupported(paste0("DATASET: '", ds, "' (a libref or data set options)"), line)
  d <- h$work[[tolower(ds)]]
  if (is.null(d)) abort_unsupported(paste0("DATASET: of a data set that does not exist (", toupper(ds), ")"), line)
  vars <- unique(c(h$keys, h$data))
  nms <- tolower(names(d))
  miss <- setdiff(vars, nms)
  if (length(miss)) abort_unsupported(paste0("DATASET: without the variable ", toupper(miss[1])), line)
  cols <- lapply(stats::setNames(vars, vars), function(v) d[[match(v, nms)]])
  for (v in vars) {
    pv <- pdv[[v]]
    if (is.character(pv) != is.character(cols[[v]])) abort_unsupported(paste0("DATASET: variable ", toupper(v), " of another type than in the PDV"), line)
    if (is.character(pv)) {
      lp <- nchar(pv, "bytes")
      ld <- h$lengths[[v]]
      if (!identical(as.integer(ld), as.integer(lp))) cols[[v]] <- sas_rt_char(cols[[v]], lp)
    }
  }
  for (i in seq_len(nrow(d))) {
    kv <- lapply(h$keys, function(v) cols[[v]][[i]])
    names(kv) <- h$keys
    code <- hash_key_code(kv)
    if (!is.null(h$entries[[code]])) next
    dv <- lapply(h$data, function(v) cols[[v]][[i]])
    names(dv) <- h$data
    h$entries[[code]] <- list(key = kv, data = dv)
  }
  # its read NOTE comes before those of the step's SET and MERGE data sets
  # [measured]; the readers' counts skip it
  sas_rt_note_read(d, tolower(ds))
  rt <- rt_diag()
  rt$reads[[length(rt$reads)]]$hash <- TRUE
  invisible()
}

#' Create a hash iterator object (DECLARE HITER) over hash object `h`
#' @export
#' @keywords internal
sas_rt_hiter_new <- function(h, .line = NULL) {
  it <- new.env(parent = emptyenv())
  it$kind <- "hiter"
  it$hash <- h
  it$cur <- NULL
  it$rc <- 0
  it$line <- .line
  h$iters <- c(h$iters, list(it))
  it
}

# The entries of a hash object in its order (ORDERED: 'a' or 'd'): the key
# codes.
hash_ordered_codes <- function(h, line) {
  if (h$ordered == "no") abort_unsupported("a hash iterator over a hash object without ORDERED: (the internal order of its entries)", line)
  ents <- h$entries
  if (!length(ents)) return(character())
  keys <- do.call(rbind, lapply(unname(ents), function(e) as.data.frame(e$key, stringsAsFactors = FALSE)))
  names(ents)[sas_order(keys, names(keys), rep(h$ordered == "d", ncol(keys)))]
}

# FIRST, LAST, NEXT and PREV of a hash iterator: the data variables of the
# entry into the PDV [measured]. NEXT before FIRST is FIRST, PREV before LAST
# is LAST; past the last (first) entry the call returns 160038, leaves the
# PDV and starts over (the next NEXT is FIRST); an empty hash object returns
# 160038.
hiter_call <- function(it, method, pdv, args) {
  line <- it$line
  if (length(args)) abort_unsupported(paste0("arguments of the hash iterator method ", toupper(method)), line)
  if (!method %in% c("first", "last", "next", "prev")) abort_unsupported(paste0("the hash iterator method ", toupper(method)), line)
  h <- it$hash
  if (!h$done) abort_unsupported(paste0("the ", toupper(method), " method of an iterator before DEFINEDONE of its hash object"), line)
  codes <- hash_ordered_codes(h, line)
  it$rc <- 0
  if (is.null(it$cur) && method == "next") method <- "first"
  if (is.null(it$cur) && method == "prev") method <- "last"
  k <- if (is.null(it$cur)) NA_integer_ else match(it$cur, codes)
  if (!is.null(it$cur) && is.na(k)) abort_unsupported("a hash iterator whose current entry is no longer in its hash object", line)
  pos <- switch(method, first = 1L, last = length(codes), "next" = k + 1L, prev = k - 1L)
  if (!length(codes) || pos < 1L || pos > length(codes)) {
    it$rc <- hash_rc_missing_key
    it$cur <- NULL
    return(pdv)
  }
  it$cur <- codes[pos]
  e <- h$entries[[it$cur]]
  pdv[h$data] <- e$data[h$data]
  pdv
}

#' The data set a hash object's OUTPUT method wrote (`name`), as a tibble
#'
#' The entries at the time of the last OUTPUT to that data set.
#' @export
#' @keywords internal
sas_rt_hash_written <- function(h, name) {
  w <- if (is.environment(h)) h$written[[name]]
  if (is.null(w)) abort_unsupported(paste0("a hash object OUTPUT to WORK.", toupper(name), " that did not run"))
  sas_rt_rows_to_tibble(w$rows, w$proto)
}

#' The return code of the last method call of a hash object
#' @export
#' @keywords internal
sas_rt_hash_rc <- function(h) h$rc

#' The number of entries of a hash object (NUM_ITEMS)
#' @export
#' @keywords internal
sas_rt_hash_num_items <- function(h) as.numeric(length(h$entries))
