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
    "2y"  = c("change_ln_spot", "change_ois_2y",  "change_forward_2y"),
    "5y"  = c("change_ln_spot", "change_ois_5y",  "change_forward_5y"),
    "10y" = c("change_ln_spot", "change_ois_10y", "change_forward_10y")
  )

  shocks |>
    purrr::set_names() |>
    purrr::map(function(shock) {
      tenors |>
        purrr::map(function(response_cols) {
          run_slp(
            data          = data,
            response_cols = response_cols,
            shock_col     = shock,
            P = P,
            H = H
          )
        })
    })
}




# MPS responses -----------------------------------------------------------
mps_all_slp <- run_all_slp(sa_model_data_tbl)

# Access individual plots, e.g.:
slp_plots <- function(data){
  data |>
  purrr::imap(function(shock_results, shock_name) {
    shock_results$`2y`$plot /
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
                        tenors        = c("2y", "5y", "10y"),
                        response_cols = c("change_ln_spot", "change_forward_2y", "change_forward_5y", "change_forward_10y"),
                        H = 12) {
  
  horizons <- seq(0, H)
  shocks   <- names(all_slp)
  
  plot_data <- tidyr::expand_grid(tenor = tenors, response = response_cols) |>
    purrr::pmap(function(tenor, response) {
      shocks |>
        purrr::set_names() |>
        purrr::map(function(shock) {
          ir <- all_slp[[shock]][[tenor]]$results[[response]]
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
        scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
        labs(x = "Horizon", y = "Response", title = resp,
             color = "Surprise", fill = "Surprise") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
}

comparison_plots <- compare_slp(mps_all_slp)

# Access by response variable:
comparison_plots$change_ln_spot
comparison_plots$change_forward_2y
comparison_plots$change_forward_5y
comparison_plots$change_forward_10y

# Data releases -----------------------------------------------------------
releases_all_slp <- 
  run_all_slp(data_releases_tbl, 
            shocks = c(
              "CPI YoY",
              "GDP YoY",
              "South Africa Unemployment"
            ),
            P = 10) |> 
  slp_plots()

# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


