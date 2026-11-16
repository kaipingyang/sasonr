# displayed code of an L1, an L2 and an L3 step

    Code
      cat(tr$steps[[length(tr$steps)]]$code, sep = "\n")
    Output
      # ---- DATA B (SAS line 6) -- level L1
      # L1: no cross-row state
      .d <- sas_ds(work, "a") |> sas_note_read("a") |>
        dplyr::mutate(
          w = NA_real_,
          c = "   ",
          w = sas_arith(sas_arith(v, 2, "*", .at = c(6L, 22L)), 0.1, "+", .at = c(6L, 26L), .gen_a = TRUE),
          c = dplyr::if_else(sas_true(sas_gt(v, 1)), sas_char("big", 3L), c)
        )
      work$b <- .d |>
        dplyr::select(g, v, w, c)

---

    Code
      cat(tr$steps[[length(tr$steps)]]$code, sep = "\n")
    Output
      # ---- DATA B (SAS line 6) -- level L2
      # L2: BY-group processing (FIRST./LAST.); sum statement (RETAIN + accumulate); unconditional LAG/DIF
      .d <- sas_ds(work, "a") |> sas_note_read("a") |>
        sas_check_sorted("g", FALSE) |>
        dplyr::mutate(
          first.g = sas_first(g),
          last.g = sas_last(g),
          p = NA_real_
        ) |>
        dplyr::group_by(.grp = cumsum(first.g)) |>
        dplyr::mutate(
          t = sas_cumsum(v, 0, dplyr::n())
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          p = sas_lag_vec(v, 1L)
        )
      work$b <- .d |>
        dplyr::select(g, v, t, p)

---

    Code
      cat(tr$steps[[length(tr$steps)]]$code, sep = "\n")
    Output
      # ---- DATA B (SAS line 1) -- level L3
      # L3: iterative DO loop; LEAVE/CONTINUE; explicit OUTPUT
      pdv <- list(
        i = NA_real_,
        x = NA_real_,
        `_error_` = 0,
        `_n_` = NA_real_
      )
      .proto <- pdv
      .dump <- sas_dump_spec(c("i", "x"), c("i", "x"), list())
      .reset <- pdv[c("i", "x")]
      .out1 <- list()
      .vars1 <- c("i", "x")
      .n_iter <- 0L
      repeat {
        .n_iter <- .n_iter + 1L
        pdv[names(.reset)] <- .reset
        pdv$`_n_` <- .n_iter
        pdv$`_error_` <- 0
        .status <- tryCatch({
          .from2 <- 1
          .to2 <- 3
          .by2 <- 1
          pdv$i <- .from2
          .first2 <- TRUE
          repeat {
            if (!.first2) {
              pdv$i <- pdv$i + .by2
            }
            .first2 <- FALSE
            if (!sas_do_test(pdv$i, .to2, .by2)) break
            repeat {
              .s1 <- pdv$i
              if (sas_true(sas_in(.s1, list(2)))) {
                break
              } else {
              }
              break
            }
            pdv$x <- sas_div(pdv$i, 3, .at = c(1L, 74L))
            pdv <- sas_err_sync(pdv)
            .out1[[length(.out1) + 1L]] <- pdv[.vars1]
          }
          "ok"
        }, sasonr_delete = function(e) "delete", sasonr_stop = function(e) "stop", sasonr_return = function(e) "ok")
        pdv <- sas_err_sync(pdv)
        .log <- c(.log, sas_iter_dump(pdv, .dump))
        if (.status == "stop") break
        break
      }
      work$b <- sas_rows_to_tibble(.out1, .proto[.vars1])

