# displayed code of an L1, an L2 and an L3 step

    Code
      cat(tr$steps[[length(tr$steps)]]$code, sep = "\n")
    Output
      # ---- DATA B (SAS line 6) -- level L1
      # L1: no cross-row state
      .df <- sas_ds(work, "a") |>
        sas_note_read("a") |>
        dplyr::mutate(
          c = "   ",
          w = sas_arith(
            sas_arith(v, 2, "*", .at = c(6L, 22L)),
            0.1,
            "+",
            .at = c(6L, 26L),
            .gen_a = TRUE
          ),
          c = dplyr::if_else(sas_true(sas_gt(v, 1)), sas_char("big", 3L), c)
        )
      work$b <- .df |> dplyr::select(g, v, w, c)

---

    Code
      cat(tr$steps[[length(tr$steps)]]$code, sep = "\n")
    Output
      # ---- DATA B (SAS line 6) -- level L2
      # L2: BY-group processing (FIRST./LAST.); sum statement (RETAIN + accumulate); unconditional LAG/DIF
      .df <- sas_ds(work, "a") |>
        sas_note_read("a") |>
        sas_check_sorted("g", FALSE) |>
        dplyr::mutate(
          first.g = sas_first(g),
          p = NA_real_
        ) |>
        dplyr::group_by(.grp = cumsum(first.g)) |>
        dplyr::mutate(t = sas_cumsum(v, 0, dplyr::n())) |>
        dplyr::ungroup() |>
        dplyr::mutate(p = sas_lag_vec(v, 1L))
      work$b <- .df |> dplyr::select(g, v, t, p)

---

    Code
      cat(tr$steps[[length(tr$steps)]]$code, sep = "\n")
    Output
      # ---- DATA B (SAS line 1) -- level L3
      # L3: iterative DO loop; LEAVE/CONTINUE; explicit OUTPUT
      pdv <- list(i = NA_real_, x = NA_real_, `_error_` = 0, `_n_` = NA_real_)
      .st <- sas_step_state(
        pdv,
        reset = c("i", "x"),
        outputs = list(c("i", "x")),
        dump = sas_dump_spec(c("i", "x"), c("i", "x"), list())
      )
      repeat {
        pdv <- sas_iter_begin(.st, pdv)
        .status <- sas_iter_run({
          pdv$i <- 1
          while (sas_do_test(pdv$i, 3, 1)) {
            repeat {
              .s1 <- pdv$i
              if (sas_true(sas_in(.s1, list(2)))) {
                break
              }
              break
            }
            pdv$x <- sas_div(pdv$i, 3, .at = c(1L, 74L))
            sas_write_pdv(.st, pdv)
            pdv$i <- pdv$i + 1
          }
        })
        pdv <- sas_err_sync(pdv)
        .log <- c(.log, sas_iter_dump(pdv, .st))
        if (sas_iter_done(.st, .status)) break
      }
      work$b <- sas_step_output(.st, 1L)

