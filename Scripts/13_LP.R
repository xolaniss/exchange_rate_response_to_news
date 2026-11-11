# Description
# LPs - XS October 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
source(here("Functions", "lproj.R"))
source(here("Functions", "run_slp.R"))
# Import -------------------------------------------------------------
mps_tbl <- 
  read_rds( file = here("Outputs", "artifacts_model_data.rds")) |> 
  pluck(1)

data_releases_tbl <-  
  read_rds( file = here("Outputs", "artifacts_model_data.rds")) |> 
  pluck(2)

# Run all LP IRFs -----------------------------------------------------------------
run_all_slp <- function(data,
                        shocks = c(
                          "target",
                          "forward_guidance",
                          "central_bank_information",
                          "country_risk"
                        ),
                        P = 4, H = 12) {
  tenors <- list(
    "1y"  = c("change_ois_1y",  "change_forward_1y"),
    "2y"  = c("change_ois_2y",  "change_forward_2y"),
    "3y"  = c("change_ois_3y",  "change_forward_3y"),
    "4y"  = c("change_ois_4y",  "change_forward_4y"),
    "5y"  = c("change_ois_5y",  "change_forward_5y"),
    "10y" = c("change_ois_10y", "change_forward_10y")
  )

  shocks |>
    purrr::set_names() |>
    purrr::map(function(shock) {
      fit <- function(response_cols) {
        run_slp(
          data          = data,
          response_cols = response_cols,
          shock_col     = shock,
          P = P,
          H = H
        )
      }
      # Spot has no tenor, so it is estimated once per shock
      c(
        list(spot = fit("change_ln_spot")),
        purrr::map(tenors, fit)
      )
    })
}




# MPS responses -----------------------------------------------------------
mps_all_slp <- run_all_slp(mps_tbl)

# Access individual plots, e.g.:
slp_plots <- function(data){
  data |>
  purrr::imap(function(shock_results, shock_name) {
      shock_results$spot$plot /
      shock_results$`1y`$plot /
      shock_results$`2y`$plot /
      shock_results$`3y`$plot /
      shock_results$`4y`$plot /
      shock_results$`5y`$plot /
      shock_results$`10y`$plot +
      patchwork::plot_annotation(title = shock_name)
  })
}


mps_plots <- slp_plots(mps_all_slp)

target_gg <- mps_plots$target
fg_gg <- mps_plots$forward_guidance
cbi_gg <- mps_plots$central_bank_information
country_risk <- mps_plots$country_risk



# Comparison plots: same response across all surprises --------------------
compare_slp <- function(all_slp,
                        tenors        = c("1y", "2y", "3y", "4y","5y","10y"),
                        response_cols = c(
                          "change_ln_spot",
                          "change_ois_1y",
                          "change_ois_2y",
                          "change_ois_3y",
                          "change_ois_4y",
                          "change_ois_5y",
                          "change_ois_10y",
                          "change_forward_10y",
                          "change_forward_1y",
                          "change_forward_2y",
                          "change_forward_3y",
                          "change_forward_4y",
                          "change_forwards_5y",
                          "change_forward_10y"
                        ),
                        H = 12) {
  
  horizons <- seq(0, H)
  shocks   <- names(all_slp)

  # Label follows how the IRFs were scaled in run_slp()
  per_unit <- all_slp[[1]]$spot$results[[1]]$per_unit
  y_label  <- if (isTRUE(per_unit)) "Response per 1 pp shock" else "Response per 1 SD shock"
  
  plot_data <- tidyr::expand_grid(tenor = tenors, response = response_cols) |>
    purrr::pmap(function(tenor, response) {
      shocks |>
        purrr::set_names() |>
        purrr::map(function(shock) {
          # Spot is stored once per shock under `spot`, not under each tenor
          ir <- if (response == "change_ln_spot") {
            if (tenor != tenors[1]) return(NULL)
            all_slp[[shock]]$spot$results[[response]]
          } else {
            all_slp[[shock]][[tenor]]$results[[response]]
          }
          if (is.null(ir)) return(NULL)
          tibble::tibble(
            Horizon  = horizons,
            Response = as.vector(ir$ir.opt),
            Lower    = ir$irc[, 1],
            Upper    = ir$irc[, 2]
          )
        }) |>
        dplyr::bind_rows(.id = "shock") |>
        dplyr::mutate(tenor    = factor(tenor, levels = tenors),
                      response = response)
    }) |>
    dplyr::bind_rows()
  
  # One plot per response — facet columns = tenor (in order), overlay = shock
  response_cols |>
    purrr::set_names() |>
    purrr::map(function(resp) {
      plot_data |>
        dplyr::filter(response == resp) |>
        drop_na() |> 
        ggplot(aes(x = Horizon, y = Response, color = shock, fill = shock)) +
        geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, color = NA) +
        geom_line(linewidth = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
        facet_wrap(~ shock, scales = "free_y", ncol = 2) +
        scale_x_continuous(breaks = c(0, 6, 12)) +
        scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
        labs(x = "Horizon", y = y_label, title = resp,
             color = "Surprise", fill = "Surprise") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
}

comparison_plots <- compare_slp(mps_all_slp)

# Access by response variable:
comparison_plots$change_ln_spot
comparison_plots$change_ois_1y
comparison_plots$change_ois_2y
comparison_plots$change_ois_3y
comparison_plots$change_ois_4y
comparison_plots$change_ois_5y
comparison_plots$change_ois_10y
comparison_plots$change_forward_1y
comparison_plots$change_forward_2y
comparison_plots$change_forward_3y
comparison_plots$change_forward_4y
comparison_plots$change_forward_5y
comparison_plots$change_forward_10y

# OIS slope: tilt vs parallel shift -----------------------------------------
# Slope of the SA-US OIS differential (10y minus 2y), in pp. A parallel shift of
# the differential curve implies a zero slope response; a tilt implies non-zero.
mps_slope_tbl <- 
  mps_tbl |> 
  mutate(change_ois_slope = change_ois_10y - change_ois_2y)

run_slope_slp <- function(data,
                          shocks = c(
                            "target",
                            "forward_guidance",
                            "central_bank_information",
                            "country_risk"
                          ),
                          P = 4, H = 12) {
  shocks |>
    purrr::set_names() |>
    purrr::map(function(shock) {
      run_slp(
        data          = data,
        response_cols = "change_ois_slope",
        shock_col     = shock,
        P = P,
        H = H
      )
    })
}

mps_slope_slp <- run_slope_slp(mps_slope_tbl)

mps_slope_irf_tbl <- 
  mps_slope_slp |> 
  purrr::imap(function(res, shock) {
    ir <- res$results$change_ois_slope
    tibble::tibble(
      shock    = shock,
      Horizon  = seq_along(ir$ir.opt) - 1,
      Response = as.vector(ir$ir.opt),
      Lower    = ir$irc[, 1],
      Upper    = ir$irc[, 2]
    )
  }) |> 
  dplyr::bind_rows()

mps_slope_gg <- 
  ggplot(mps_slope_irf_tbl, aes(x = Horizon, y = Response, color = shock, fill = shock)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~ shock, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = c(0, 6, 12)) +
  labs(x = "Horizon", y = "Response of OIS slope (pp) per 1 pp shock",
       title = "OIS slope (10y minus 2y)") +
  theme_minimal() +
  theme(legend.position = "none")

mps_slope_gg
# OIS level (2y) response -----------------------------------------------------
# Tidy IRFs for one response/tenor across all shocks in an run_all_slp() result
extract_irf <- function(all_slp, tenor, response) {
  all_slp |>
    purrr::imap(function(shock_results, shock) {
      ir <- shock_results[[tenor]]$results[[response]]
      tibble::tibble(
        shock    = shock,
        Horizon  = seq_along(ir$ir.opt) - 1,
        Response = as.vector(ir$ir.opt),
        Lower    = ir$irc[, 1],
        Upper    = ir$irc[, 2]
      )
    }) |>
    dplyr::bind_rows()
}

plot_irf_by_shock <- function(irf_tbl, y_label, title) {
  ggplot(irf_tbl, aes(x = Horizon, y = Response, color = shock, fill = shock)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    facet_wrap(~ shock, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = c(0, 6, 12)) +
    labs(x = "Horizon", y = y_label, title = title) +
    theme_minimal() +
    theme(legend.position = "none")
}

mps_ois_2y_irf_tbl <- extract_irf(mps_all_slp, "2y", "change_ois_2y")

mps_ois_2y_gg <- 
  plot_irf_by_shock(
    mps_ois_2y_irf_tbl,
    y_label = "Response of 2y OIS differential (pp) per 1 pp shock",
    title   = "2y OIS differential (SA - US)"
  )

mps_ois_2y_gg

# Data releases -----------------------------------------------------------
releases_slp_results <- 
  run_all_slp(data_releases_tbl, 
            shocks = c(
              "CPI YoY",
              "GDP YoY",
              "South Africa Unemployment"
            ),
            P = 4)

releases_all_slp <- slp_plots(releases_slp_results)

## Comparison plots: same response across all data releases
releases_comparison_plots <- compare_slp(releases_slp_results)

# Access by response variable:
releases_comparison_plots$change_ln_spot
releases_comparison_plots$change_forward_1y
releases_comparison_plots$change_forward_2y
releases_comparison_plots$change_forward_3y
releases_comparison_plots$change_forward_4y
releases_comparison_plots$change_forward_5y
releases_comparison_plots$change_forward_10y



# Export ---------------------------------------------------------------
artifacts_lp <- list (
  mps_plots                 = mps_plots,
  mps_comparison_plots      = comparison_plots,
  releases_plots            = releases_all_slp,
  releases_comparison_plots = releases_comparison_plots,
  mps_slope_irf_tbl         = mps_slope_irf_tbl,
  mps_slope_plot            = mps_slope_gg,
  mps_ois_2y_irf_tbl        = mps_ois_2y_irf_tbl,
  mps_ois_2y_plot           = mps_ois_2y_gg

)

write_rds(artifacts_lp, file = here("Outputs", "artifacts_lp.rds"))


