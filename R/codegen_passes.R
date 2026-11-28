# Readability passes for the code of a vectorised (L1/L2) DATA step.
#
# A pass is a pure function `pass(stages, facts) -> stages` on the stages of
# one step (see add_stage(): `list(kind, code)` with language objects), run
# after the stages are built and before render_pipeline() turns them into the
# code that runs and the code that is shown, so both see the same rewrite.  A
# pass rewrites a pattern only when the result is provably the same: same
# values and attributes of every column read later or written out, and the
# same log.  Option `sasonr.passes` names the passes that run (default: all of
# codegen_pass_table()); `character()` turns them all off, and the code is
# then the code built without passes.
#
# `facts`: `an` (the step analysis) and `reads_after` (the names that code
# after the pipeline reads from `.df`: PUT lines, the output condition and the
# output columns).

codegen_pass_table <- function() {
  data.frame(
    id = c("by_first_value", "unused_first_last"),
    description = c(
      "a retained numeric variable set only at FIRST. of a BY group from a column: one grouped value per BY group",
      "drop FIRST./LAST. columns that no later expression reads"
    ),
    stringsAsFactors = FALSE
  )
}

codegen_pass_fun <- function(id) {
  switch(id,
    by_first_value = pass_by_first_value,
    unused_first_last = pass_unused_first_last
  )
}

# The passes that run, in table order.
codegen_passes_on <- function() {
  ids <- codegen_pass_table()$id
  sel <- getOption("sasonr.passes", ids)
  if (!is.character(sel)) {
    rlang::abort("Option `sasonr.passes` must be a character vector of pass ids.", class = "sasonr_error")
  }
  bad <- setdiff(sel, ids)
  if (length(bad)) {
    rlang::abort(paste0("Unknown pass in option `sasonr.passes`: ", paste(bad, collapse = ", "), "."),
                 class = "sasonr_error")
  }
  ids[ids %in% sel]
}

apply_codegen_passes <- function(stages, facts) {
  for (id in codegen_passes_on()) stages <- codegen_pass_fun(id)(stages, facts)
  stages
}

# Helpers -----------------------------------------------------------------

# Names that make an expression read columns by string or selection, so the
# set of columns of the data matters to it.
pass_all_cols <- c("pick", "across", "c_across", "cur_data", "cur_data_all", "everything", "all_of", "any_of",
                   ".data", "starts_with", "ends_with", "matches", "contains", "num_range", "where", "get", "mget")

# Every name and string an expression mentions.
pass_used_names <- function(e) {
  strings <- function(x) {
    if (is.character(x)) return(x)
    if (is.call(x) || is.pairlist(x)) return(unlist(lapply(as.list(x), strings)))
    character()
  }
  c(all.names(e), strings(e))
}

stage_used_names <- function(st) {
  if (st$kind == "mutate") return(unlist(lapply(st$code, pass_used_names)))
  pass_used_names(st$code)
}

# TRUE when the stages after position `k` (and the code after the pipeline)
# read a column by string or selection, so dropping or adding a column could
# change what they see.
reads_all_after <- function(stages, k, facts) {
  later <- stages[seq_along(stages) > k]
  used <- c(unlist(lapply(later, stage_used_names)), facts$reads_after)
  any(pass_all_cols %in% used)
}

# Pass: unused FIRST./LAST. columns ----------------------------------------

# `first.x = sas_rt_first(...)` / `last.x = sas_rt_last(...)` in a mutate stage that
# no later stage and no code after the pipeline reads: the output select()
# drops the column, and sas_rt_first()/sas_rt_last() write nothing to the log.
pass_unused_first_last <- function(stages, facts) {
  for (k in seq_along(stages)) {
    st <- stages[[k]]
    if (st$kind != "mutate") next
    nms <- names(st$code)
    flag <- vapply(seq_along(st$code), function(j) {
      e <- st$code[[j]]
      grepl("^(first|last)\\.", nms[j]) && is.call(e) &&
        (identical(e[[1]], quote(sas_rt_first)) || identical(e[[1]], quote(sas_rt_last)))
    }, logical(1))
    if (!any(flag)) next
    if (reads_all_after(stages, k, facts)) next
    later <- c(unlist(lapply(stages[seq_along(stages) > k], stage_used_names)), facts$reads_after)
    drop <- logical(length(nms))
    for (j in which(flag)) {
      # read by a later column of the same mutate, or by any later stage
      same <- unlist(lapply(st$code[seq_along(st$code) > j], pass_used_names))
      drop[j] <- !nms[j] %in% c(same, later) && !nms[j] %in% nms[seq_along(nms) > j]
    }
    if (!any(drop)) next
    stages[[k]]$code <- st$code[!drop]
  }
  Filter(function(st) st$kind != "mutate" || length(st$code) > 0, stages)
}

# Pass: value set at FIRST. of a BY group --------------------------------

# retain v; if first.b then v = x;  is built as
#   .cv_v_1 = x, .cs_v_1 = sas_rt_true(first.b)                  (stage k)
#   v = sas_rt_carry(list(.cv_v_1), list(.cs_v_1), init)         (stage k + 1)
# and becomes
#   group_by(.grp = cumsum(first.b)) |> mutate(v = as.numeric(dplyr::first(x))) |> ungroup()
# when
#   * the carry plan of v has this one assignment only (so it is the only
#     assignment of v in the step) and its condition is first.b, b a BY variable;
#   * first.b is the sas_rt_first() column of the input stages and no stage before
#     the carry removes rows (no filter) or regroups them: then row 1 has
#     first.b = 1, the runs of cumsum(first.b) are the runs that start at a row
#     with first.b = 1, and on every row sas_rt_carry() returns x on the first row
#     of its run, so the RETAIN initial value is never used;
#   * x is a column name (evaluated once per group instead of per row, it
#     writes nothing and cannot fail) and v is numeric: sas_rt_carry() returns a
#     plain double vector (the initial value is a double without attributes,
#     and x is assigned into it), as as.numeric() does for every numeric,
#     logical, factor or date column;
#   * no later code reads columns by string or selection (it would see
#     .grp in place of .cv_v_1 / .cs_v_1).
# Runs of cumsum(first.b) are used rather than group_by() on the BY variables:
# they follow sas_rt_first() exactly (BY NOTSORTED, special missing values).
pass_by_first_value <- function(stages, facts) {
  an <- facts$an
  plan <- an$carry_plan
  if (!length(plan) || is.null(an$by)) return(stages)
  for (v in names(plan)) {
    p <- plan[[v]]
    if (length(p$sets) != 1L) next
    if (!identical(an$info[[v]]$type, "num")) next
    cv <- sprintf(".cv_%s_1", v)
    cs <- sprintf(".cs_%s_1", v)
    k <- which(vapply(stages, function(st) st$kind == "mutate" && identical(names(st$code), c(cv, cs)), logical(1)))
    if (length(k) != 1L || k >= length(stages)) next
    nxt <- stages[[k + 1L]]
    if (nxt$kind != "mutate" || !identical(names(nxt$code), v)) next
    carry <- nxt$code[[1]]
    if (!is.call(carry) || !identical(carry[[1]], quote(sas_rt_carry)) ||
        !identical(carry[[2]], rlang::call2("list", rlang::sym(cv))) ||
        !identical(carry[[3]], rlang::call2("list", rlang::sym(cs)))) next
    x <- stages[[k]]$code[[1]]
    cond <- stages[[k]]$code[[2]]
    if (!is.symbol(x)) next
    if (!is.call(cond) || !identical(cond[[1]], quote(sas_rt_true)) || length(cond) != 2L || !is.symbol(cond[[2]])) next
    flag <- as.character(cond[[2]])
    b <- sub("^first\\.", "", flag)
    if (!startsWith(flag, "first.") || !b %in% an$by$vars) next
    if (!first_flag_is_input(stages, k, flag, an$by$vars)) next
    if (reads_all_after(stages, k + 1L, facts)) next
    new <- list(
      list(kind = "raw", code = rlang::call2(quote(dplyr::group_by), .grp = rlang::call2("cumsum", rlang::sym(flag)))),
      list(kind = "mutate", code = stats::setNames(list(
        rlang::call2("as.numeric", rlang::call2(quote(dplyr::first), x))), v)),
      list(kind = "raw", code = quote(dplyr::ungroup()))
    )
    stages <- c(stages[seq_len(k - 1L)], new, stages[seq_along(stages) > k + 1L])
  }
  stages
}

# `flag` (first.b) is set once, by the sas_rt_first() call of the BY variables up
# to b, in a mutate stage before stage `k`, and every stage before `k` is a
# mutate or the sort check (no row is removed, added or regrouped).
first_flag_is_input <- function(stages, k, flag, by) {
  b <- sub("^first\\.", "", flag)
  keys <- rlang::call2("sas_rt_first", !!!rlang::syms(by[seq_len(match(b, by))]))
  set_at <- integer()
  for (i in seq_len(k - 1L)) {
    st <- stages[[i]]
    if (st$kind == "mutate") {
      if (flag %in% names(st$code)) {
        if (!identical(st$code[[flag]], keys) || sum(names(st$code) == flag) != 1L) return(FALSE)
        set_at <- c(set_at, i)
      }
      next
    }
    if (st$kind == "raw" && is.call(st$code) && identical(st$code[[1]], quote(sas_rt_check_sorted))) next
    return(FALSE)
  }
  length(set_at) == 1L
}
