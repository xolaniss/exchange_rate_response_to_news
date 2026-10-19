run_slp <-
  function(data, response_cols, shock_col, P = 20, h1 = 0, H = 20,
           lambda = c(0.0001, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0, 9, 10) / 1000) {

    # Helper: prepare estimation data for one response column
    prepare_data <- function(r_col) {
      lp_data <- data |>
        dplyr::select(
          response = dplyr::all_of(r_col),
          shock    = dplyr::all_of(shock_col)
        ) |>
        tidyr::drop_na()

      y <- lp_data$response
      x <- lp_data$shock

      w_mat    <- cbind(y, lagmatrix(cbind(y, x), P))
      full_mat <- na.omit(cbind(y, x, w_mat))

      list(
        y = full_mat[, 1],
        x = full_mat[, 2],
        w = full_mat[, 3:ncol(full_mat)]
      )
    }

    # Helper: run LP estimation
    run_estimation <- function(est_data) {
      fit_init <- lproj(
        y = est_data$y, x = est_data$x, w = est_data$w,
        const = TRUE, type = "smooth", H = H, h1 = h1, r = 2, lambda = lambda
      )
      fit_cv <- lproj.cv(fit_init, 10)
      lproj.conf(fit_cv, fit_cv$idx.opt)
    }

    horizons <- seq(h1, H)

    # Run LP for each response column
    results <- response_cols |>
      purrr::set_names() |>
      purrr::map(function(r_col) {
        run_estimation(prepare_data(r_col))
      })

    # Build long plot data — one row per horizon per response
    plot_data <- results |>
      purrr::imap(function(ir, r_col) {
        tibble::tibble(
          Horizon  = horizons,
          Response = as.vector(ir$ir.opt),
          Type     = r_col,
          Lower    = ir$irc[, 1],
          Upper    = ir$irc[, 2]
        )
      }) |>
      dplyr::bind_rows()

    # Color palette — supports up to 3 response columns
    palette <- c("#dd4124", "#edd746", "#4f86c6")
    colors  <- setNames(palette[seq_along(response_cols)], response_cols)

    # Plot
    p <- ggplot(plot_data, aes(x = Horizon, y = Response, color = Type, fill = Type)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper),
                  alpha = 0.2, color = NA) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      facet_wrap(~ Type, scales = "free_y", ncol = length(response_cols)) +
      labs(x = "Horizon", y = "Response", title = "", color = "") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = c(0, 6, 12)) +
      scale_y_continuous(labels = scales::label_number(accuracy = 0.001))

    list(plot = p, results = results)
  }
