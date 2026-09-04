# Description
# preliminary models

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
robust_model <- 
  function(model) {
    coeftest(
      model, vcov = NeweyWest(model)
    ) |> 
      tidy()
  }


news_models <- 
  function(data,
           surprises = c(
             "target_models"                   = "target",
             "forward_guidance_models"         = "forward_guidance",
             "central_bank_information_models" = "central_bank_information",
             "country_risk_models"             = "country_risk"
           ),
           predictors = c(
             "change_ln_spot_models" = "change_ln_spot",
             "change_ois_2y_models" = "change_ois_2y",
             "change_ois_5y_models" = "change_ois_5y",
             "change_forward_2y_models" = "change_forward_2y",
             "change_forward_5y_models" = "change_forward_5y"
           )) {
    surprises |> 
      purrr::map(function(surprise) {
        predictors |> 
          purrr::map(~ lm(data = data, formula = reformulate(.x, surprise))) |> 
          purrr::map(robust_model) |> 
          bind_rows(.id = "model")
      }) |> 
      bind_rows(.id = "surprise_type")
  }

# Import -------------------------------------------------------------
model_data <- read_rds(here("Outputs", "artifacts_model_data.rds"))


# MPC models  -----------------------------------------------------------------
mpc_announcement_days_tbl <- model_data |>  pluck(1)
mpc_surprises_models <- news_models(mpc_announcement_days_tbl)  
mpc_surprises_models |>  print(n = 100)


## News models -------------------------------------------------------------
news_announcement_days_tbl <- model_data |>  pluck(2) |> 
  janitor::clean_names()

data_releases_models_tbl <- 
  news_models(
  data = news_announcement_days_tbl,
  surprises = c(
    "cpi_models" = "cpi_yo_y",
    "gdp_models" = "gdp_yo_y",
    "south_africa_unemployment_models" = "south_africa_unemployment"
  ),
  predictors = c(
    "change_ln_spot_models" = "change_ln_spot",
    "change_ois_2y_models" = "change_ois_2y",
    "change_ois_5y_models" = "change_ois_5y",
    "change_forward_2y_models" = "change_forward_2y",
    "change_forward_5y_models" = "change_forward_5y"
  )
)

data_releases_models_tbl |> 
  print(n = 100)


# Export ---------------------------------------------------------------
artifacts_models <- list (
  mpc_surprises_models = mpc_surprises_models,
  data_releases_models_tbl = data_releases_models_tbl
)

write_rds(artifacts_models, file = here("Outputs", "artifacts_models.rds"))


