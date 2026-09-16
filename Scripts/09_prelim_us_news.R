# Description
# preliminary models

# Preliminaries -----------------------------------------------------------
library(here)
options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
source(here("Functions", "model_functions.R"))

# Import -------------------------------------------------------------
model_data <- read_rds(here("Outputs", "artifacts_model_data.rds"))


# MPC models  -----------------------------------------------------------------
us_mpc_announcement_days_tbl <- model_data |>  pluck(3)
us_mpc_surprises_models <- news_models(us_mpc_announcement_days_tbl, 
                                       surprises = c(
                                         "target_models"                   = "jaracinski_ffr",
                                         "forward_guidance_models"         = "jaracinski_fg",
                                         "central_bank_information_models" = "jaracinski_cbi",
                                         "lsap_models"                     = "jaracinski_lsap"
                                       ))  |> 
  mutate(across(-c(surprise_type, model, term), ~ round(.x, 9))) |> 
  filter(!term == "(Intercept)")

us_mpc_surprises_models |>  print(n = 100)


# Residual models ---------------------------------------------------------
residuals_tbl <- 
  news_model_residuals(
  us_mpc_announcement_days_tbl, 
  surprises = c(
    "target_models"                   = "jaracinski_ffr",
    "forward_guidance_models"         = "jaracinski_fg",
    "central_bank_information_models" = "jaracinski_cbi",
    "lsap_models"                     = "jaracinski_lsap"))

## Residual plots ----------------------------------------------------------
spot_resids_tbl <- 
  residuals_tbl |> 
  dplyr::filter(str_detect(model, "ln_spot")) |> 
  dplyr::select(surprise_type, date, resid_spot = .resid)

ois_resids_tbl <-  
  residuals_tbl  |> 
  dplyr::filter(str_detect(model, "ois")) |> 
  dplyr::select(surprise_type, date, ois_model = model, resid_ois = .resid) |> 
  dplyr::mutate(ois_model = str_remove(ois_model, "change_|_models"))

scatter_plot_data_tbl <- 
  dplyr::inner_join(spot_resids, ois_resids, by = c("surprise_type", "date")) |> 
  dplyr::mutate(
    surprise_type = str_remove(surprise_type, "_models"),
    ois_model     = toupper(str_replace(ois_model, "ois_", "OIS "))
  ) |> 
  dplyr::mutate(
    ois_model     = str_remove(ois_model, "_MODELS") |> str_replace("OIS ", "OIS ") |> 
                    factor(levels = c("OIS 2Y", "OIS 5Y", "OIS 10Y")),
    surprise_type = str_replace_all(surprise_type, "_", " ") |> str_to_title()
  )

ggplot(scatter_plot_data_tbl, aes(x = resid_ois, y = resid_spot)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, color = "steelblue") +
  facet_grid(surprise_type ~ ois_model, scales = "free_x",
             labeller = labeller(surprise_type = label_wrap_gen(15))) +
  labs(x = "OIS residual", y = "Spot residual",
       title = "Residual-on-residual: spot vs OIS after partialling out surprise factor") +
  theme(strip.text.y = element_text(size = 7)) +
  theme_minimal()

resid

# Export ---------------------------------------------------------------
artifacts_us_news_models <- list (
  us_mpc_surprises_models = us_mpc_surprises_models
)

write_rds(artifacts_us_news_models, file = here("Outputs", "artifacts_us_news_models.rds"))


