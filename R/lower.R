# Lowering of an analysed DATA step to the step IR (`sas_dsir`).
#
# This file collects the declarative parts of a step into slots. The slots are
# filled from the analysed statements (`sas_analyze()` output), never from the
# raw AST, so type and attribute facts stay where the analyser computes them.
# sas_analyze() attaches them as `an$dsir`; code generation reads WHERE,
# KEEP/DROP/RENAME and the output data sets from there.

#' Declarative slots of an analysed DATA step
#'
#' Walks `an$stmts` in statement order (descending into IF/ELSE, DO and
#' SELECT bodies) and reads the existing fields of `an`; nothing is analysed
#' again.
#'
#' `$slots` holds:
#' \describe{
#'   \item{source}{One entry per reading statement (SET, MERGE, INPUT) in
#'     statement order: `kind` (`"set"`, `"merge"`, `"input"`), `line`,
#'     `in_block` (inside an IF/DO/SELECT body), `reader` (index into
#'     `an$readers`), `datasets` (SET/MERGE: one entry per data set with
#'     `name`, `options` as parsed and analysed (KEEP=/DROP= items, RENAME=
#'     pairs, WHERE= analysed expression, IN=, FIRSTOBS=/OBS=), `vars` (input
#'     variables after KEEP=/DROP=) and `out_names` (after RENAME=); empty for
#'     INPUT), `end`, `nobs` (SET/MERGE END=/NOBS= variables or NULL) and
#'     `stmt` (the analysed statement itself).}
#'   \item{where}{`stmts`: every WHERE statement in order, each `list(expr,
#'     line, in_block)` with the analysed condition; `effective`: the
#'     condition that applies (the last WHERE statement replaces earlier
#'     ones; NULL when there is none).}
#'   \item{select_rename}{`stmts`: KEEP, DROP and RENAME statements in order,
#'     each `list(kind, line, in_block, items)` (`items`: the variable-list
#'     items of KEEP/DROP, the named old = new vector of RENAME); `keep`,
#'     `drop`, `rename`: the combined statement lists the analyser holds;
#'     `per_output`: per output data set `list(name, vars, out_names)` as
#'     `output_specs()` resolves them.}
#'   \item{attrs}{LENGTH, FORMAT, INFORMAT, LABEL, ATTRIB, RETAIN and ARRAY
#'     statements in statement order (PDV order depends on it), each
#'     `list(kind, line, in_block, content)`. `content` is the statement's
#'     parsed specification: `specs` (LENGTH/FORMAT/INFORMAT), `labels` and
#'     `texts` (LABEL), `groups` (ATTRIB; RETAIN groups with `vars`, `paren`,
#'     the parsed `init` nodes and `init_values`, the constants they denote),
#'     and for ARRAY the analysed array (`an$arrays[[name]]`: elements, type,
#'     length, dimension, initial values).}
#'   \item{by}{NULL without a BY statement, else `list(vars, descending,
#'     notsorted, groupformat, line)`.}
#'   \item{outputs}{One entry per output data set of the DATA statement:
#'     `name`, `options` (as parsed), `line`, and the resolved `vars` and
#'     `out_names` (from `an$outputs`); empty for DATA _NULL_.}
#' }
#'
#' @param an The result of [sas_analyze()].
#' @return An object of class `sas_dsir` with element `slots`.
#' @noRd
dsir_slots <- function(an) {
  acc <- new.env(parent = emptyenv())
  acc$source <- list()
  acc$where <- list()
  acc$sel <- list()
  acc$attrs <- list()
  n_reader <- 0L
  visit <- function(s, in_block) {
    k <- s$stmt
    if (k %in% c("set", "merge", "input")) {
      n_reader <<- n_reader + 1L
      dss <- if (k == "input") list() else lapply(s$datasets, function(d) {
        list(name = d$name, options = d$options, vars = d$vars, out_names = d$out_names)
      })
      acc$source <- c(acc$source, list(list(
        kind = k, line = s$line, in_block = in_block, reader = n_reader, datasets = dss,
        end = s$end, nobs = s$nobs, stmt = s
      )))
    } else if (k == "by") {
      # only a BY statement at the top level of the step is hoisted
      if (in_block) abort_unsupported("a BY statement inside a block", s$line)
    } else if (k == "where") {
      acc$where <- c(acc$where, list(list(expr = s$cond, line = s$line, in_block = in_block)))
    } else if (k %in% c("keep", "drop", "rename")) {
      items <- if (k == "rename") s$pairs else s$items
      acc$sel <- c(acc$sel, list(list(kind = k, line = s$line, in_block = in_block, items = items)))
    } else if (k %in% c("length", "format", "informat", "label", "attrib", "retain", "array")) {
      content <- switch(k,
        length = , format = , informat = list(specs = s$specs),
        label = list(labels = s$labels, texts = s$texts),
        attrib = list(groups = s$groups),
        retain = list(groups = lapply(s$groups, function(g) {
          g$init_values <- lapply(g$init, const_value)
          g
        })),
        array = an$arrays[[s$name]]
      )
      acc$attrs <- c(acc$attrs, list(list(kind = k, line = s$line, in_block = in_block, content = content)))
    }
    for (ch in stmt_children(s)) visit(ch, TRUE)
  }
  for (s in an$stmts) visit(s, FALSE)

  wheres <- acc$where
  # the combined statement lists: KEEP/DROP items in order, RENAME pairs (a
  # later pair for the same old name replaces an earlier one)
  sel <- list(keep = list(), drop = list(), rename = character())
  for (x in acc$sel) {
    if (x$kind == "rename") sel$rename[names(x$items)] <- x$items
    else sel[[x$kind]] <- c(sel[[x$kind]], x$items)
  }
  resolved <- output_specs(an, sel)
  by <- if (is.null(an$by)) NULL else list(
    vars = an$by$vars, descending = an$by$descending,
    # the parser rejects NOTSORTED and GROUPFORMAT
    notsorted = FALSE, groupformat = FALSE, line = an$by$line
  )
  outs <- lapply(seq_along(resolved), function(i) {
    o <- an$step$outputs[[i]]
    r <- resolved[[i]]
    list(name = o$name, options = o$options, line = o$line, vars = r$vars, out_names = r$out_names)
  })
  slots <- list(
    source = acc$source,
    where = list(
      stmts = wheres,
      effective = slot_where_effective(wheres)$expr
    ),
    select_rename = list(
      stmts = acc$sel, keep = sel$keep, drop = sel$drop, rename = sel$rename,
      per_output = lapply(resolved, function(o) o[c("name", "vars", "out_names")])
    ),
    attrs = acc$attrs,
    by = by,
    outputs = outs
  )
  structure(list(slots = slots), class = "sas_dsir")
}

#' The WHERE statement that applies
#'
#' A later WHERE statement replaces an earlier one (DATA step and PROC alike).
#'
#' @param wheres WHERE statements (or slot entries) in statement order.
#' @return The last element of `wheres`, or NULL when there is none.
#' @noRd
slot_where_effective <- function(wheres) {
  if (length(wheres) == 0) return(NULL)
  wheres[[length(wheres)]]
}
