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
  mutate(across(-c(surprise_type, model, term), ~ round(.x, 9)))

us_mpc_surprises_models |>  print(n = 100)

# Export ---------------------------------------------------------------
artifacts_us_news_models <- list (
  mpc_surprises_models = mpc_surprises_models,
  data_releases_models_tbl = data_releases_models_tbl
)

write_rds(artifacts_us_news_models, file = here("Outputs", "artifacts_us_news_models.rds"))


