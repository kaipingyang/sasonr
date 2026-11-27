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
#'     variables after KEEP=/DROP=), `out_names` (after RENAME=) and `opts`
#'     (the normalised option record, `ds_opts_record()`); empty for INPUT),
#'     `end`, `nobs` (SET/MERGE END=/NOBS= variables or NULL) and
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
        list(name = d$name, options = d$options, vars = d$vars, out_names = d$out_names,
             opts = ds_opts_record(d))
      })
      acc$source <- c(acc$source, list(list(
        kind = k, line = s$line, in_block = in_block, reader = n_reader, datasets = dss,
        end = s$end, nobs = s$nobs, stmt = s
      )))
    } else if (k == "by") {
      # a BY statement belongs to its SET or MERGE statement (the analysis
      # attaches it), wherever it stands
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

# Control-flow graph of the executable body ----------------------------------

#' Lower the executable body of an analysed DATA step to basic blocks
#'
#' Builds the control-flow graph of the step from `an$stmts`. The
#' declarative statements (WHERE, BY, KEEP/DROP/RENAME, LENGTH, FORMAT,
#' INFORMAT, LABEL, ATTRIB, RETAIN, ARRAY, DATALINES) are in the slots and give
#' no instruction. FILE and INFILE are step-wide in the engine (one
#' destination, one source, chosen before the first iteration), so they are
#' not instructions either. The lowering follows the row code of the L3
#' generator (`gen_row_step()`) statement by statement; nothing in the engine
#' uses the result yet.
#'
#' Each block is `list(id, instrs, term, loop)`; `loop` is the id of the
#' innermost loop whose body holds the block (NULL outside loops).
#'
#' Instructions are `list(op, line, col, ...)` with `stmt` (the analysed
#' statement) and:
#' \describe{
#'   \item{assign}{`target` (variable, array element or a synthetic
#'     `loop_temp`/`select_temp` node), `expr`; `substr = TRUE` with `pos`,
#'     `len` for the left-hand SUBSTR.}
#'   \item{sum}{`target`, `expr` (sum statement).}
#'   \item{read}{`reader` (index into `an$readers` and the source slot),
#'     `kind` (`"set"`, `"merge"`).}
#'   \item{input}{`reader`, `items`.}
#'   A read or INPUT instruction ends its block with `branch(eof, stop,
#'     next)` (`cond` a node `eof` with the reader, `eof = TRUE` on the
#'     terminator): at the end of the input the step stops there; the stop
#'     block has `term = "stop"`, `eof = TRUE`.
#'   \item{output}{`targets` (data set names; empty: every output),
#'     `index` (positions in the output slot, NA for an unknown name).}
#'   \item{put}{`items`.}
#'   \item{call}{`routine`, `args`, `targets`.}
#' }
#'
#' Terminators are `list(term, line, ...)`: `jump(to)`, `branch(cond,
#' true, false)` (`cond` taken as `sas_true()`), `return`, `delete`, `stop`,
#' `abort(kind, msg)` (only `kind = "select"`: no WHEN matched and no
#' OTHERWISE) and `end_iteration` (the end of the body).
#'
#' Loops: `$loops[[id]]` is `list(id, kind, var, line, head, body, latch,
#' exit)`. DO WHILE tests at the head, DO UNTIL in the latch after the body;
#' the iterative DO assigns the start value and evaluates TO and BY once
#' (`loop_temp` targets), the head tests the bound (`loop_test`) and then the
#' WHILE condition, the latch tests UNTIL and increments (`loop_next`); a DO
#' value list evaluates every value once before the loop (`loop_temp` "v1",
#' "v2", ...), keeps a position (`loop_temp` "index"), and the body starts by
#' assigning the current value (`loop_value`, whose `values` are the
#' `loop_temp` nodes). LEAVE jumps to the loop exit,
#' CONTINUE to the latch (DO WHILE / UNTIL: the test block). SELECT lowers
#' to a chain of `when_test` branches.
#'
#' Statements after an unconditional transfer (DELETE, STOP, RETURN, LEAVE,
#' CONTINUE) are unreachable; their blocks are removed and the lines of the
#' executable statements that start in them (also those giving only a
#' terminator, and IF/DO/SELECT heads) are kept in `$unreachable`. A LAG/DIF call in such a statement keeps its site with
#' `block = NA`.
#'
#' @param an The result of [sas_analyze()].
#' @return The `sas_dsir` object of `an` (its slots) with `blocks` (named by
#'   id), `entry`, `labels` (statement labels: always empty, the parser
#'   rejects them), `loops`, `lag_sites` (data frame `id, fn, n, block`),
#'   and `unreachable`. A LEAVE leaves the innermost DO loop or SELECT group
#'   (inside a SELECT it jumps to the end of the SELECT).
#' @noRd
dsir_lower <- function(an) {
  d <- an$dsir %||% dsir_slots(an)
  g <- new.env(parent = emptyenv())
  g$blocks <- list()
  g$n <- 0L
  g$loops <- list()
  g$n_select <- 0L
  g$reader <- 0L
  g$cur <- NULL
  g$starts <- list()
  out_names <- vapply(d$slots$outputs, `[[`, "", "name")

  new_block <- function(loop = NULL) {
    g$n <- g$n + 1L
    id <- paste0("B", g$n)
    g$blocks[[id]] <- list(id = id, instrs = list(), term = NULL, loop = loop)
    id
  }
  cur_loop <- function(ctx) if (length(ctx$loops)) ctx$loops[[length(ctx$loops)]]$id
  emit <- function(op, s, ...) {
    ins <- list(op = op, line = s$line %||% NA_integer_, col = s$col %||% NA_integer_, ...)
    ins$stmt <- s
    b <- g$blocks[[g$cur]]
    b$instrs <- c(b$instrs, list(ins))
    g$blocks[[g$cur]] <- b
  }
  term <- function(tk, s, ...) {
    g$blocks[[g$cur]]$term <- list(term = tk, line = s$line %||% NA_integer_, ...)
  }
  # an unconditional transfer: what follows starts in a new (unreachable) block
  transfer <- function(tk, s, ctx, ...) {
    term(tk, s, ...)
    g$cur <- new_block(cur_loop(ctx))
  }
  goto <- function(id) g$cur <- id
  loop_temp <- function(k, name) list(node = "loop_temp", loop = k, name = name)

  # a read or INPUT that finds the input exhausted stops the step
  # (sas_set_next() at the end of its rows): the block ends in a branch on `eof`
  end_of_input <- function(s, ctx) {
    loop <- cur_loop(ctx)
    stop_b <- new_block(loop)
    next_b <- new_block(loop)
    term("branch", s, cond = list(node = "eof", reader = g$reader), true = stop_b, false = next_b, eof = TRUE)
    goto(stop_b)
    term("stop", s, eof = TRUE)
    goto(next_b)
  }
  lower_stmt <- function(s, ctx) {
    if (!s$stmt %in% c(decl_stmts, "datalines", "file", "infile", "null")) {
      g$starts[[length(g$starts) + 1L]] <- c(block = g$cur, line = as.character(s$line %||% NA_integer_))
    }
    switch(s$stmt,
      assign = emit("assign", s, target = s$target, expr = s$value),
      substr_assign = emit("assign", s, target = list(node = "var", name = s$var), expr = s$value,
                           substr = TRUE, pos = s$pos, len = s$len),
      sum = emit("sum", s, target = s$target, expr = s$value),
      set = ,
      merge = {
        g$reader <- g$reader + 1L
        emit("read", s, reader = g$reader, kind = s$stmt)
        end_of_input(s, ctx)
      },
      input = {
        g$reader <- g$reader + 1L
        emit("input", s, reader = g$reader, items = s$items)
        end_of_input(s, ctx)
      },
      output = emit("output", s, targets = s$targets,
                    index = if (length(s$targets)) match(s$targets, out_names) else seq_along(out_names)),
      put = emit("put", s, items = s$items),
      call = emit("call", s, routine = s$routine, args = s$args, targets = s$targets),
      "if" = {
        loop <- cur_loop(ctx)
        then_b <- new_block(loop)
        else_b <- if (!is.null(s[["else"]])) new_block(loop)
        join <- new_block(loop)
        term("branch", s, cond = s$cond, true = then_b, false = else_b %||% join)
        goto(then_b)
        lower_stmt(s$then, ctx)
        term("jump", s, to = join)
        if (!is.null(else_b)) {
          goto(else_b)
          lower_stmt(s[["else"]], ctx)
          term("jump", s, to = join)
        }
        goto(join)
      },
      subset_if = {
        loop <- cur_loop(ctx)
        keep <- new_block(loop)
        del <- new_block(loop)
        term("branch", s, cond = s$cond, true = keep, false = del)
        goto(del)
        term("delete", s)
        goto(keep)
      },
      delete = transfer("delete", s, ctx),
      stop = transfer("stop", s, ctx),
      "return" = transfer("return", s, ctx),
      # LEAVE leaves the innermost DO loop or SELECT group
      leave = transfer("jump", s, ctx, to = if (isTRUE(ctx$in_select)) ctx$select_join else ctx$loops[[length(ctx$loops)]]$exit),
      continue = transfer("jump", s, ctx, to = ctx$loops[[length(ctx$loops)]]$latch),
      do = lower_do(s, ctx),
      select = lower_select(s, ctx),
      # declarative (slots) or step-wide (FILE, INFILE): no instruction
      where = , by = , keep = , drop = , rename = , length = , format = , informat = ,
      label = , attrib = , retain = , array = , datalines = , file = , infile = , null = NULL,
      rlang::abort(sprintf("dsir_lower(): no lowering for the %s statement (line %s)", toupper(s$stmt), s$line %||% "?"),
           .internal = TRUE)
    )
    invisible()
  }
  lower_body <- function(stmts, ctx) for (s in stmts) lower_stmt(s, ctx)

  lower_do <- function(s, ctx) {
    if (s$kind == "block") return(lower_body(s$body, ctx))
    k <- length(g$loops) + 1L
    outer <- cur_loop(ctx)
    lp <- list(id = k, kind = s$kind, var = s$var, line = s$line)
    g$loops[[k]] <- lp
    if (s$kind %in% c("iter", "list")) {
      if (s$kind == "iter") {
        # the start, TO and BY are evaluated before the index is assigned
        emit("assign", s, target = loop_temp(k, "from"), expr = s$from)
        emit("assign", s, target = loop_temp(k, "to"), expr = s$to)
        emit("assign", s, target = loop_temp(k, "by"), expr = s$by %||% list(node = "num", value = 1, text = "1"))
        emit("assign", s, target = list(node = "var", name = s$var), expr = loop_temp(k, "from"))
      } else {
        # every value is evaluated once, before the first iteration
        for (j in seq_along(s$values)) emit("assign", s, target = loop_temp(k, paste0("v", j)), expr = s$values[[j]])
        emit("assign", s, target = loop_temp(k, "index"), expr = list(node = "loop_first", loop = k))
      }
      head <- new_block(outer)
      term("jump", s, to = head)
      wu <- s$while_until
      wtest <- if (identical(wu$kind, "while")) new_block(outer)
      body <- new_block(k)
      latch <- new_block(outer)
      incr <- if (identical(wu$kind, "until")) new_block(outer) else latch
      exit <- new_block(outer)
      goto(head)
      term("branch", s, cond = list(node = "loop_test", loop = k, kind = s$kind), true = wtest %||% body, false = exit)
      if (!is.null(wtest)) {
        goto(wtest)
        term("branch", s, cond = wu$cond, true = body, false = exit)
      }
      goto(latch)
      if (identical(wu$kind, "until")) term("branch", s, cond = wu$cond, true = exit, false = incr)
      goto(incr)
      if (s$kind == "iter") {
        emit("assign", s, target = list(node = "var", name = s$var), expr = list(node = "loop_next", loop = k))
      } else {
        emit("assign", s, target = loop_temp(k, "index"), expr = list(node = "loop_next", loop = k))
      }
      term("jump", s, to = head)
      goto(body)
      if (s$kind == "list") {
        emit("assign", s, target = list(node = "var", name = s$var),
             expr = list(node = "loop_value", loop = k,
                         values = lapply(seq_along(s$values), function(j) loop_temp(k, paste0("v", j)))))
      }
    } else {
      # DO WHILE: test at the top; DO UNTIL: the body runs once before the test
      test <- new_block(outer)
      body <- new_block(k)
      exit <- new_block(outer)
      term("jump", s, to = if (s$kind == "while") test else body)
      goto(test)
      if (s$kind == "while") term("branch", s, cond = s$cond, true = body, false = exit)
      else term("branch", s, cond = s$cond, true = exit, false = body)
      head <- if (s$kind == "while") test else body
      latch <- test
      goto(body)
    }
    lp$head <- head
    lp$body <- body
    lp$latch <- latch
    lp$exit <- exit
    g$loops[[k]] <- lp
    inner <- ctx
    inner$loops <- c(ctx$loops, list(lp))
    inner$in_select <- FALSE
    lower_body(s$body, inner)
    term("jump", s, to = latch)
    goto(exit)
  }

  lower_select <- function(s, ctx) {
    g$n_select <- g$n_select + 1L
    k <- g$n_select
    loop <- cur_loop(ctx)
    join <- new_block(loop)
    ctx$in_select <- TRUE
    ctx$select_join <- join
    if (!is.null(s$expr)) emit("assign", s, target = list(node = "select_temp", select = k), expr = s$expr)
    no_match <- function() {
      if (isTRUE(s$has_otherwise)) lower_stmt(s$otherwise, ctx)
      else transfer("abort", s, ctx, kind = "select", msg = "no WHEN clause matched and there is no OTHERWISE clause")
    }
    for (w in s$whens) {
      body <- new_block(loop)
      nxt <- new_block(loop)
      term("branch", w, cond = list(node = "when_test", select = k, has_expr = !is.null(s$expr), values = w$values),
           true = body, false = nxt)
      goto(body)
      lower_stmt(w$body, ctx)
      term("jump", w, to = join)
      goto(nxt)
    }
    no_match()
    term("jump", s, to = join)
    goto(join)
  }

  entry <- new_block()
  goto(entry)
  lower_body(an$stmts, list(loops = list()))
  term("end_iteration", list(line = NA_integer_))

  # drop the unreachable blocks
  seen <- character()
  todo <- entry
  while (length(todo)) {
    id <- todo[1]
    todo <- todo[-1]
    if (id %in% seen) next
    seen <- c(seen, id)
    todo <- c(todo, dsir_succ(g$blocks[[id]]$term))
  }
  dead <- setdiff(names(g$blocks), seen)
  # every executable statement that starts in a removed block, also those
  # that give only a terminator (DELETE, STOP, ...) or a head (IF, DO, SELECT)
  starts <- vctrs::vec_rbind(!!!g$starts, .ptype = data.frame(block = character(), line = character()))
  unreachable <- sort(unique(as.integer(starts[starts[, "block"] %in% dead, "line"])))
  lags <- dsir_lag_sites(g$blocks)
  lags$block[lags$block %in% dead] <- NA_character_
  d$blocks <- g$blocks[seen[order(as.integer(sub("^B", "", seen)))]]
  d$entry <- entry
  d$labels <- list()
  d$loops <- g$loops
  d$lag_sites <- lags
  d$unreachable <- unreachable
  d
}

#' Successor block ids of a terminator
#' @noRd
dsir_succ <- function(t) {
  switch(t$term,
    jump = t$to,
    branch = unique(c(t$true, t$false)),
    character()
  )
}

#' LAG/DIF call sites of the blocks
#'
#' A site is a call the analyser numbered (`e$site`, `e$lag`), found in the
#' expressions of an instruction or a branch condition.
#' @return Data frame `id, fn ("lag"/"dif"), n, block`, ordered by id.
#' @noRd
dsir_lag_sites <- function(blocks) {
  acc <- list()
  walk <- function(x, b) {
    if (!is.list(x)) return(invisible())
    if (!is.null(x$site) && !is.null(x$lag)) {
      acc[[length(acc) + 1L]] <<- data.frame(id = vctrs::vec_cast(x$site, integer()), fn = x$lag$kind,
                                             n = vctrs::vec_cast(x$lag$n, integer()), block = b, stringsAsFactors = FALSE)
    }
    for (y in x) walk(y, b)
  }
  for (b in blocks) {
    for (i in b$instrs) walk(i[setdiff(names(i), "stmt")], b$id)
    if (identical(b$term$term, "branch")) walk(b$term$cond, b$id)
  }
  if (!length(acc)) return(data.frame(id = integer(), fn = character(), n = integer(), block = character(),
                                      stringsAsFactors = FALSE))
  out <- vctrs::vec_rbind(!!!acc)
  out <- out[!duplicated(out$id), ]
  out <- out[order(out$id), ]
  rownames(out) <- NULL
  out
}

#' Compact text of a lowered step (debugging)
#'
#' One line per block header, instruction and terminator, e.g.
#' `B2 (loop 1):`, `  3: assign x = (x + 1)`, `  -> branch (x > 3) ? B3 : B4`.
#'
#' @param x The result of [dsir_lower()].
#' @return A character vector (also printed with `cat()` when `print = TRUE`).
#' @noRd
dsir_dump <- function(x, print = FALSE) {
  L <- character()
  for (b in x$blocks) {
    L <- c(L, paste0(b$id, if (identical(b$id, x$entry)) " (entry)", if (!is.null(b$loop)) sprintf(" (loop %d)", b$loop), ":"))
    for (i in b$instrs) {
      what <- switch(i$op,
        assign = , sum = paste0(dsir_expr_text(i$target), if (i$op == "sum") " + " else " = ", dsir_expr_text(i$expr),
                                if (isTRUE(i$substr)) " [substr]"),
        read = sprintf("%s reader %d", i$kind, i$reader),
        input = sprintf("reader %d", i$reader),
        output = if (length(i$targets)) paste(i$targets, collapse = " ") else "(all)",
        call = i$routine,
        ""
      )
      L <- c(L, sprintf("  %s: %s %s", i$line %||% "?", i$op, what))
    }
    t <- b$term
    L <- c(L, paste0("  -> ", switch(t$term,
      jump = paste("jump", t$to),
      branch = sprintf("branch %s ? %s : %s", dsir_expr_text(t$cond), t$true, t$false),
      abort = sprintf("abort %s", t$kind),
      t$term
    )))
  }
  if (nrow(x$lag_sites)) {
    L <- c(L, paste0("lag sites: ", paste(sprintf("%d %s%d in %s", x$lag_sites$id, x$lag_sites$fn, x$lag_sites$n,
                                                  x$lag_sites$block), collapse = "; ")))
  }
  if (print) cat(L, sep = "\n")
  invisible(L)
}

# A short text of an expression node for dsir_dump().
dsir_expr_text <- function(e) {
  if (is.null(e)) return("NULL")
  switch(e$node %||% "?",
    var = e$name,
    num = e$text %||% format(e$value),
    str = paste0("'", e$value, "'"),
    missing = e$code %||% ".",
    binary = , compare = sprintf("(%s %s %s)", dsir_expr_text(e$lhs), e$op, dsir_expr_text(e$rhs)),
    unary = sprintf("%s %s", e$op, dsir_expr_text(e$arg)),
    call = sprintf("%s(%s)", e$name, paste(vapply(e$args, dsir_expr_text, ""), collapse = ", ")),
    array_ref = sprintf("%s{%s}", e$name, paste(vapply(e$index, dsir_expr_text, ""), collapse = ", ")),
    loop_temp = sprintf(".%s%d", e$name, e$loop),
    loop_test = sprintf("loop_test(%d)", e$loop),
    loop_first = sprintf("loop_first(%d)", e$loop),
    loop_next = sprintf("loop_next(%d)", e$loop),
    loop_value = sprintf("loop_value(%d)", e$loop),
    select_temp = sprintf(".sel%d", e$select),
    eof = sprintf("eof(%d)", e$reader),
    when_test = sprintf("when(%s)", paste(vapply(e$values, dsir_expr_text, ""), collapse = ", ")),
    paste0("<", e$node, ">")
  )
}
