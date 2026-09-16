# Description
# residual on residual for SA - XS Sep 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
source(here("Functions", "model_functions.R"))

# Import -------------------------------------------------------------
model_data <- read_rds(here("Outputs", "artifacts_model_data.rds"))

# MPC models  -----------------------------------------------------------------
mpc_announcement_days_tbl <- model_data |>  pluck(1)

## Residual models ---------------------------------------------------------
sa_residuals_tbl <- 
  news_model_residuals(mpc_announcement_days_tbl)

sa_residual_gg <-
  sa_residuals_tbl |> 
  ggplot(aes(x = date, y = .resid)) + 
  geom_line() + 
  facet_wrap(surprise_type ~ model, scale = "free_y") +
  theme_minimal()

sa_residual_models_tbl <- 
  residual_models(sa_residuals_tbl)

## Residual plots ----------------------------------------------------------
sa_spot_resids_tbl <- 
  sa_residuals_tbl |> 
  dplyr::filter(str_detect(model, "ln_spot")) |> 
  dplyr::select(surprise_type, date, resid_spot = .resid)

sa_ois_resids_tbl <-  
  sa_residuals_tbl  |> 
  dplyr::filter(str_detect(model, "ois")) |> 
  dplyr::select(surprise_type, date, ois_model = model, resid_ois = .resid) |> 
  dplyr::mutate(ois_model = str_remove(ois_model, "change_|_models"))

sa_scatter_plot_data_tbl <- 
  dplyr::inner_join(sa_spot_resids_tbl, sa_ois_resids_tbl, by = c("surprise_type", "date")) |> 
  dplyr::mutate(
    surprise_type = str_remove(surprise_type, "_models"),
    ois_model     = toupper(str_replace(ois_model, "ois_", "OIS "))
  ) |> 
  dplyr::mutate(
    ois_model     = str_remove(ois_model, "_MODELS") |> str_replace("OIS ", "OIS ") |> 
      factor(levels = c("OIS 2Y", "OIS 5Y", "OIS 10Y")),
    surprise_type = str_replace_all(surprise_type, "_", " ") |> str_to_title()
  )

# Graphing ---------------------------------------------------------------
sa_residual_scatter_gg <- 
  ggplot(sa_scatter_plot_data_tbl, aes(x = resid_ois, y = resid_spot)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, color = "steelblue") +
  facet_grid(surprise_type ~ ois_model, scales = "free_x",
             labeller = labeller(surprise_type = label_wrap_gen(15))) +
  labs(x = "OIS residual", y = "Spot residual",
       title = "Residual-on-residual: spot vs OIS after partialling out surprise factor") +
  theme(strip.text.y = element_text(size = 7)) +
  theme_minimal()

# Export ---------------------------------------------------------------
artifacts_sa_residual_models <- list (
 sa_residuals_tbl = sa_residuals_tbl,
 sa_residual_models_tbl = sa_residual_models_tbl,
 sa_residual_scatter_gg = sa_residual_scatter_gg 
)

write_rds(artifacts_sa_residual_models, file = here("Outputs", "artifacts_sa_residual_models.rds"))


