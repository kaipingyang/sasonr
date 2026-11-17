# Data-flow facts of a lowered DATA step and a shadow classification.
#
# dsir_facts() answers the questions of spec 01 section 4.2 on the control-flow
# graph of dsir_lower(); dsir_level() derives the L1/L2/L3 level from them.
# sas_analyze() takes the level from dsir_level(); classify_step() builds the
# L2 plans and decides only where dsir_level() returns NA.

#' Data-flow facts of a lowered DATA step
#'
#' Every instruction, branch condition and iteration end becomes an event with
#' the variables it reads (`uses`), the variables it surely sets (`kills`) and
#' those it may set (`may`: an array element, a CALL routine argument). The
#' end of an iteration (`end_iteration`, `return`) reads the variables of every
#' output data set when the step has no OUTPUT statement (implicit output); an
#' `output` instruction reads those of its targets. A read sets the variables
#' of its data sets, the IN= and END= variables and FIRST./LAST.; an INPUT
#' sets its variables.
#'
#' \describe{
#'   \item{explicit_output}{An `output` instruction anywhere.}
#'   \item{cross_row_live}{Variables read on some path from the entry before
#'     they are set, that the PDV does not reset at the top of an iteration
#'     (retained, sum and input data set variables; not `_N_`, `_ERROR_`)
#'     and that the step sets somewhere: values carried from the previous
#'     row that are used. A temporary array is one variable `.ta_<name>`.}
#'   \item{exposed}{Per variable of `cross_row_live`, the events (`"B3:2"`:
#'     block, position) where it is read so.}
#'   \item{read_dominates}{One read (SET, MERGE, INPUT) that is the first
#'     instruction of its block, whose block dominates every block that holds
#'     an instruction or a branch, outside any loop and executed on every row
#'     that completes the iteration; NA without a read.}
#'   \item{conditional}{Sites not executed exactly once on every row that
#'     completes the iteration (not post-dominating the entry, or inside a
#'     loop): `lag` (site ids), `sum` (target variables), `output` (block
#'     ids), `read` (TRUE/FALSE/NA).}
#'   \item{needs_state_machine}{Always FALSE: the parser rejects LINK and
#'     GOTO.}
#'   \item{iteration_ends}{Blocks ending in `end_iteration` or `return` (the
#'     two are equivalent iteration ends).}
#' }
#' The other elements (`events`, `succ`, `dom`, `pdom`, `sites`) are the
#' working data `dsir_level()` reads.
#'
#' Post-dominance is taken over the rows that complete the iteration: the
#' end-of-input edge of a read and the blocks ending in DELETE, STOP or the
#' SELECT error do not count as paths.
#'
#' @param ir The result of [dsir_lower()].
#' @param an The result of [sas_analyze()].
#' @return A list of class `sas_dsir_facts`.
#' @noRd
dsir_facts <- function(ir, an) {
  blocks <- ir$blocks
  ids <- names(blocks)
  out_vars <- lapply(ir$slots$outputs, `[[`, "vars")
  all_instrs <- vctrs::list_unchop(lapply(blocks, `[[`, "instrs"), ptype = list(), name_spec = rlang::zap())
  explicit_output <- any(vapply(all_instrs, function(i) i$op == "output", NA))

  arr_vars <- function(name) {
    a <- an$arrays[[name]]
    if (is.null(a) || isTRUE(a$temporary)) paste0(".ta_", name) else a$elems
  }
  reads <- function(e) {
    if (is.null(e) || !is.list(e)) return(character())
    switch(e$node %||% "",
      loop_test = , loop_next = if (identical(ir$loops[[e$loop]]$kind, "iter")) ir$loops[[e$loop]]$var else character(),
      when_test = unique(vctrs::list_unchop(lapply(e$values, reads), ptype = character(), name_spec = rlang::zap())),
      loop_value = , loop_first = , loop_temp = , select_temp = , eof = character(),
      {
        out <- expr_reads(e)
        walk_expr(e, function(x) if (x$node == "array_ref") out <<- c(out, arr_vars(x$name)))
        unique(out)
      }
    )
  }
  has_array_ref <- FALSE
  note_arrays <- function(e) {
    if (is.list(e) && !is.null(e$node) && !e$node %in% c("loop_test", "loop_next", "when_test", "loop_value",
                                                          "loop_first", "loop_temp", "select_temp", "eof")) {
      walk_expr(e, function(x) if (x$node == "array_ref") has_array_ref <<- TRUE)
    }
    if (is.list(e) && identical(e$node, "when_test")) for (v in e$values) note_arrays(v)
  }
  read_kills <- function(i) {
    src <- ir$slots$source[[i$reader]]
    if (i$op == "input") return(input_var_names(i$stmt))
    c(vctrs::list_unchop(lapply(src$datasets, `[[`, "out_names"), ptype = character(), name_spec = rlang::zap()),
      vctrs::list_unchop(lapply(src$datasets, function(d) d$options$in_var), ptype = character(), name_spec = rlang::zap()),
      src$end,
      if (!is.null(an$by)) paste0(c("first.", "last."), rep(an$by$vars, each = 2)))
  }
  instr_event <- function(i) {
    ev <- list(uses = character(), kills = character(), may = character(), op = i$op, instr = i)
    switch(i$op,
      assign = {
        t <- i$target
        ev$uses <- c(reads(i$expr), if (isTRUE(i$substr)) c(t$name, reads(i$pos), reads(i$len)))
        if (t$node == "var") {
          if (isTRUE(i$substr)) ev$may <- t$name else ev$kills <- t$name
        } else if (t$node == "array_ref") {
          ev$uses <- c(ev$uses, vctrs::list_unchop(lapply(t$index, reads), ptype = character(), name_spec = rlang::zap()))
          ev$may <- arr_vars(t$name)
          has_array_ref <<- TRUE
        }
        note_arrays(i$expr)
      },
      sum = {
        ev$uses <- c(i$target$name, reads(i$expr))
        ev$kills <- i$target$name
        note_arrays(i$expr)
      },
      read = ev$kills <- read_kills(i),
      input = {
        ev$kills <- read_kills(i)
        ev$uses <- vctrs::list_unchop(lapply(Filter(function(it) !is.null(it$v), i$items), function(it) reads(it$v)), ptype = character(), name_spec = rlang::zap())
      },
      output = ev$uses <- unique(vctrs::list_unchop(out_vars[stats::na.omit(i$index)], ptype = character(), name_spec = rlang::zap())),
      put = ev$uses <- unique(vctrs::list_unchop(lapply(Filter(Negate(is.null), stmt_exprs(i$stmt)), reads), ptype = character(), name_spec = rlang::zap())),
      call = {
        ev$uses <- unique(vctrs::list_unchop(lapply(i$args, reads), ptype = character(), name_spec = rlang::zap()))
        ev$may <- vctrs::list_unchop(lapply(i$targets, function(t) if (t$node == "var") t$name), ptype = character(), name_spec = rlang::zap())
      }
    )
    ev$uses <- unique(ev$uses)
    ev
  }
  implicit_uses <- if (explicit_output) character() else unique(vctrs::list_unchop(out_vars, ptype = character(), name_spec = rlang::zap()))
  events <- lapply(blocks, function(b) {
    evs <- lapply(b$instrs, instr_event)
    t <- b$term
    if (t$term == "branch") {
      note_arrays(t$cond)
      evs <- c(evs, list(list(uses = reads(t$cond), kills = character(), may = character(), op = "branch")))
    } else if (t$term %in% c("end_iteration", "return")) {
      evs <- c(evs, list(list(uses = implicit_uses, kills = character(), may = character(), op = "end")))
    }
    evs
  })
  succ <- lapply(blocks, function(b) dsir_succ(b$term))

  # dominators (all paths, the end-of-input edges included)
  preds <- lapply(stats::setNames(ids, ids), function(id) ids[vapply(succ, function(s) id %in% s, NA)])
  dom <- lapply(stats::setNames(ids, ids), function(id) if (id == ir$entry) id else ids)
  repeat {
    changed <- FALSE
    for (id in setdiff(ids, ir$entry)) {
      p <- preds[[id]]
      new <- union(id, if (length(p)) Reduce(intersect, dom[p]) else character())
      if (!setequal(new, dom[[id]])) {
        dom[[id]] <- new
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  # post-dominators over the rows that complete the iteration
  is_end <- vapply(blocks, function(b) b$term$term %in% c("end_iteration", "return"), NA)
  rsucc <- lapply(blocks, function(b) {
    t <- b$term
    if (t$term == "branch" && isTRUE(t$eof)) t$false else dsir_succ(t)
  })
  pdom <- lapply(stats::setNames(ids, ids), function(id) if (is_end[[id]]) id else ids)
  repeat {
    changed <- FALSE
    for (id in ids[!is_end]) {
      s <- rsucc[[id]]
      new <- if (length(s)) union(id, Reduce(intersect, pdom[s])) else ids
      if (!setequal(new, pdom[[id]])) {
        pdom[[id]] <- new
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  once <- function(id) id %in% pdom[[ir$entry]] && is.null(blocks[[id]]$loop)

  # variables read before they are set, carried from the previous row
  exposed_of <- function(v) {
    hit <- character()
    seen <- character()
    todo <- ir$entry
    while (length(todo)) {
      b <- todo[1]
      todo <- todo[-1]
      if (b %in% seen) next
      seen <- c(seen, b)
      killed <- FALSE
      for (k in seq_along(events[[b]])) {
        e <- events[[b]][[k]]
        if (v %in% e$uses) hit <- c(hit, paste0(b, ":", k))
        if (v %in% e$kills) {
          killed <- TRUE
          break
        }
      }
      if (!killed) todo <- c(todo, succ[[b]])
    }
    hit
  }
  set_somewhere <- unique(vctrs::list_unchop(lapply(unlist(events, recursive = FALSE), function(e) c(e$kills, e$may)), ptype = character(), name_spec = rlang::zap()))
  not_reset <- Filter(function(v) {
    inf <- an$info[[v]]
    !v %in% c("_n_", "_error_") && !is.null(inf) && !(inf$kind == "data" && !isTRUE(inf$retained))
  }, an$order)
  cand <- intersect(union(not_reset, grep("^[.]ta_", set_somewhere, value = TRUE)), set_somewhere)
  exposed <- Filter(length, lapply(stats::setNames(cand, cand), exposed_of))

  # sites
  site_rows <- list()
  for (id in ids) for (k in seq_along(blocks[[id]]$instrs)) {
    i <- blocks[[id]]$instrs[[k]]
    site_rows[[length(site_rows) + 1L]] <- list(block = id, pos = k, op = i$op, line = i$line,
                                                var = if (i$op == "sum") i$target$name else NA_character_)
  }
  sites <- if (length(site_rows)) do.call(rbind, lapply(site_rows, as.data.frame, stringsAsFactors = FALSE)) else
    data.frame(block = character(), pos = integer(), op = character(), line = integer(), var = character())
  sites$once <- vapply(sites$block, once, NA)
  lags <- ir$lag_sites[!is.na(ir$lag_sites$block), ]
  rd <- sites[sites$op %in% c("read", "input"), ]
  read_dom <- if (nrow(rd) == 0) NA else if (nrow(rd) > 1) FALSE else {
    rb <- rd$block
    others <- setdiff(ids, rb)
    free <- vapply(others, function(id) {
      rb %in% dom[[id]] || (length(blocks[[id]]$instrs) == 0 && blocks[[id]]$term$term == "jump")
    }, NA)
    rd$pos == 1 && all(free) && rd$once
  }
  structure(list(
    explicit_output = explicit_output,
    cross_row_live = names(exposed),
    exposed = exposed,
    read_dominates = read_dom,
    conditional = list(
      lag = lags$id[!vapply(lags$block, once, NA)],
      sum = unique(sites$var[sites$op == "sum" & !sites$once]),
      output = unique(sites$block[sites$op == "output" & !sites$once]),
      read = if (nrow(rd) == 1) !rd$once else if (nrow(rd)) TRUE else NA
    ),
    needs_state_machine = FALSE,
    iteration_ends = ids[is_end],
    array_refs = has_array_ref,
    events = events, succ = succ, dom = dom, pdom = pdom, sites = sites, lags = lags, ir = ir
  ), class = "sas_dsir_facts")
}

#' L1/L2/L3 classification from the CFG facts
#'
#' The level `sas_analyze()` uses, derived from [dsir_facts()] (spec 01 section 4.2 "Classification becomes"): L1 when no
#' variable is carried across rows, the read dominates (or there is none)
#' and there is no conditional state site; L2 when every cross-row variable
#' and every state site is covered by a recognised pattern (sum statement
#' with an optional FIRST. reset, retained value carried forward, OUTPUT at
#' the end, unconditional LAG/DIF, BY, END=, interleaving SET); otherwise L3.
#' Limits of the vectorised generator that are not data-flow facts (arrays,
#' CALL, STOP, loops, several outputs, ...) are taken from the IR and the
#' analysis; the generators do not emit RETURN, so it forces L3. A carried
#' variable assigned under nested conditions is not decided (NA).
#'
#' @param facts The result of [dsir_facts()].
#' @param an The result of [sas_analyze()].
#' @return `list(level, reasons)`; `level` is `"L1"`, `"L2"`, `"L3"` or
#'   `NA` (`reasons` then says what is not covered).
#' @noRd
dsir_level <- function(facts, an) {
  l3 <- character()
  l2 <- character()
  na <- character()
  ir <- facts$ir
  blocks <- ir$blocks
  sites <- facts$sites
  terms <- vapply(blocks, function(b) b$term$term, "")
  readers <- an$readers
  rd <- if (length(readers)) readers[[1]]

  # limits of the vectorised generator (not data-flow facts)
  if (length(ir$slots$outputs) > 1) l3 <- c(l3, "multiple output data sets")
  if (length(readers) > 1) l3 <- c(l3, "more than one SET/MERGE/INPUT statement")
  if ("_error_" %in% an$uses) l3 <- c(l3, "_ERROR_ is referenced")
  if (!is.null(an$infile) && !is.null(an$infile$opts$end)) l3 <- c(l3, "INFILE END= variable")
  if (!is.null(rd)) {
    if (rd$stmt == "input") {
      in_vars <- input_var_names(rd)
      ptr <- vctrs::list_unchop(lapply(Filter(function(it) !is.null(it$v), rd$items), function(it) expr_reads(it$v)), ptype = character(), name_spec = rlang::zap())
      if (length(setdiff(ptr, in_vars))) l3 <- c(l3, "INPUT pointer depends on non-INPUT variables")
    }
    if (rd$stmt == "merge") l3 <- c(l3, "MERGE (match-merge reads)")
    if (!is.null(rd$nobs)) l3 <- c(l3, "NOBS= variable")
    if (!is.null(rd$end)) l2 <- c(l2, "END= variable")
    if (rd$stmt == "set" && length(rd$datasets) > 1 && !is.null(an$by)) l2 <- c(l2, "interleaving SET with BY")
  }
  if (!is.null(an$by)) l2 <- c(l2, "BY-group processing (FIRST./LAST.)")
  if (facts$array_refs) l3 <- c(l3, "array references")
  if (any(vapply(ir$slots$attrs, function(a) a$kind == "array", NA))) l3 <- c(l3, "ARRAY statement")
  if (length(ir$loops)) l3 <- c(l3, "DO loop")
  if ("abort" %in% terms) l3 <- c(l3, "SELECT without OTHERWISE (runtime error semantics)")
  if (any(sites$op == "call")) l3 <- c(l3, "CALL routine")
  if (any(vapply(blocks, function(b) b$term$term == "stop" && !isTRUE(b$term$eof), NA))) l3 <- c(l3, "STOP statement")
  if (facts$needs_state_machine) l3 <- c(l3, "LINK/GOTO (state machine)")
  if ("return" %in% terms) l3 <- c(l3, "RETURN statement")

  # reachable successors of a block (itself excluded unless on a cycle)
  reach <- function(from) {
    seen <- character()
    todo <- facts$succ[[from]]
    while (length(todo)) {
      b <- todo[1]
      todo <- todo[-1]
      if (b %in% seen) next
      seen <- c(seen, b)
      todo <- c(todo, facts$succ[[b]])
    }
    seen
  }
  # only PUT instructions and plain control flow from position `pos` of block `id` on
  tail_only <- function(id, pos) {
    b <- blocks[[id]]
    rest <- if (pos < length(b$instrs)) b$instrs[(pos + 1L):length(b$instrs)] else list()
    if (!all(vapply(rest, function(i) i$op == "put", NA))) return(FALSE)
    for (r in c(id, reach(id))) {
      bb <- blocks[[r]]
      if (r != id && !all(vapply(bb$instrs, function(i) i$op == "put", NA))) return(FALSE)
      if (!bb$term$term %in% c("jump", "end_iteration", "return")) return(FALSE)
    }
    TRUE
  }
  for (k in which(sites$op == "put")) {
    if (!tail_only(sites$block[k], sites$pos[k])) {
      l3 <- c(l3, "PUT followed by other executable statements")
      break
    }
  }

  # the read
  if (isFALSE(facts$read_dominates) && length(readers) == 1) {
    l3 <- c(l3, "the read does not dominate the step (conditional, repeated or late read)")
  }

  # LAG/DIF
  if (length(facts$conditional$lag)) l3 <- c(l3, "conditional LAG/DIF (queue updated only when executed)")
  else if (nrow(facts$lags)) l2 <- c(l2, "unconditional LAG/DIF")

  # a branch on FIRST./LAST. whose true successor is `id`
  flag_branch <- function(id, prefix = c("first.", "last.")) {
    for (p in names(blocks)) {
      t <- blocks[[p]]$term
      if (t$term == "branch" && identical(t$true, id) && !isTRUE(t$eof) && identical(t$cond$node, "var") &&
          any(startsWith(t$cond$name, prefix))) return(list(block = p, var = t$cond$name))
    }
    NULL
  }

  # explicit OUTPUT
  outs <- which(sites$op == "output")
  if (length(outs)) {
    ok <- FALSE
    if (length(outs) == 1 && length(ir$slots$outputs) <= 1) {
      o <- sites[outs, ]
      ins <- blocks[[o$block]]$instrs[[o$pos]]
      if (length(ins$targets) == 0 && is.null(blocks[[o$block]]$loop) && tail_only(o$block, o$pos)) {
        if (o$once) ok <- TRUE
        else {
          fb <- flag_branch(o$block)
          ok <- !is.null(fb) && fb$block %in% facts$pdom[[ir$entry]] && length(blocks[[o$block]]$instrs) == 1
        }
      }
    }
    if (ok) l2 <- c(l2, "OUTPUT at the end of the step") else l3 <- c(l3, "explicit OUTPUT")
  }

  # the variables carried across rows
  ev_of <- function(key) {
    p <- strsplit(key, ":", fixed = TRUE)[[1]]
    facts$events[[p[1]]][[as.integer(p[2])]]
  }
  defs_of <- function(v) {
    out <- character()
    for (id in names(blocks)) for (k in seq_along(facts$events[[id]])) {
      e <- facts$events[[id]][[k]]
      if (v %in% c(e$kills, e$may)) out <- c(out, paste0(id, ":", k))
    }
    out
  }
  blk <- function(key) sub(":.*$", "", key)
  state <- facts$cross_row_live
  # the vectorised generator emits a sum statement only as an accumulator
  if (length(setdiff(sites$var[sites$op == "sum"], state))) {
    l3 <- c(l3, "sum statement on a variable not carried across rows")
  }
  for (v in state) {
    defs <- defs_of(v)
    sum_defs <- Filter(function(k) identical(ev_of(k)$op, "sum"), defs)
    if (startsWith(v, ".ta_")) {
      l3 <- c(l3, "temporary array state")
    } else if (length(sum_defs)) {
      if (length(sum_defs) > 1) { l3 <- c(l3, "several sum statements on one variable"); next }
      sb <- blk(sum_defs)
      if (v %in% facts$conditional$sum) { l3 <- c(l3, "conditional or nested sum statement"); next }
      if (!identical(facts$exposed[[v]], sum_defs)) {
        l3 <- c(l3, paste0("sum variable ", v, " is used outside the accumulate pattern"))
        next
      }
      resets <- setdiff(defs, sum_defs)
      if (length(resets) > 1) { l3 <- c(l3, paste0("sum variable ", v, " is used outside the accumulate pattern")); next }
      if (length(resets) == 1) {
        r <- ev_of(resets)
        rb <- blk(resets)
        fb <- flag_branch(rb, "first.")
        shape <- identical(r$op, "assign") && length(r$kills) == 1 && r$instr$expr$node %in% c("num", "missing") &&
          length(blocks[[rb]]$instrs) == 1 && !is.null(fb) && is.null(blocks[[rb]]$loop) && sb %in% reach(rb)
        if (!shape) { l3 <- c(l3, paste0("sum variable ", v, " is used outside the accumulate pattern")); next }
        if (is.null(an$by) || !sub("first.", "", fb$var, fixed = TRUE) %in% an$by$vars) {
          l3 <- c(l3, "sum reset not tied to a BY variable")
          next
        }
        if (is.na(const_value(r$instr$expr))) { l3 <- c(l3, "sum reset to missing"); next }
      }
      l2 <- c(l2, "sum statement (RETAIN + accumulate)")
    } else if (v %in% an$retain_stmt_vars) {
      # carried forward: no assignment follows a read of the carried value
      # (v is not read before its first or between its assignments), every
      # assignment is `v = e` or `if c then v = e` on current-row values
      def_after <- function(key) {
        bk <- blk(key)
        pos <- as.integer(sub("^.*:", "", key))
        any(vapply(defs, function(d) (blk(d) == bk && as.integer(sub("^.*:", "", d)) > pos) || blk(d) %in% reach(bk), NA))
      }
      uses_ok <- !any(vapply(facts$exposed[[v]], def_after, NA))
      shapes <- vapply(defs, function(k) {
        e <- ev_of(k)
        b <- blk(k)
        if (!identical(e$op, "assign") || !identical(e$kills, v) || !is.null(blocks[[b]]$loop)) return("no")
        conds <- list(e$instr$expr)
        if (!b %in% facts$pdom[[ir$entry]]) {
          p <- Filter(function(id) identical(blocks[[id]]$term$true, b) && identical(blocks[[id]]$term$term, "branch"),
                      names(blocks))
          if (length(p) != 1 || !p %in% facts$pdom[[ir$entry]]) return("nested")
          # the generator's shape: `if c then v = e;` alone, without ELSE
          if (length(blocks[[b]]$instrs) != 1 || !identical(blocks[[p]]$term$false, blocks[[b]]$term$to)) return("no")
          conds <- c(conds, list(blocks[[p]]$term$cond))
        }
        bad <- any(vapply(conds, function(x) has_lag_call(x) || any(expr_reads(x) %in% c(state, v)), NA))
        if (bad) "no" else "ok"
      }, "")
      dels <- names(blocks)[terms == "delete"]
      del_between <- length(defs) > 1 && any(vapply(defs, function(k) {
        r <- reach(blk(k))
        any(dels %in% r) && any(blk(setdiff(defs, k)) %in% r)
      }, NA))
      if (!uses_ok || any(shapes == "no") || del_between) {
        l3 <- c(l3, paste0("RETAIN of assigned variable ", v))
      } else if (any(shapes == "nested")) {
        na <- c(na, paste0("carried variable ", v, " assigned under nested conditions"))
      } else {
        l2 <- c(l2, "retained value carried forward (baseline / LOCF)")
      }
    } else {
      l3 <- c(l3, paste0("cross-row variable ", v, " not covered by a pattern"))
    }
  }

  if (length(l3)) list(level = "L3", reasons = unique(l3))
  else if (length(na)) list(level = NA_character_, reasons = unique(na))
  else if (length(l2)) list(level = "L2", reasons = unique(l2))
  else list(level = "L1", reasons = "no cross-row state")
}
