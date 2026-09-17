# Description
# differential decomposition - XS Sep 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
source(here("Functions", "model_functions.R"))

# Import -------------------------------------------------------------
sa_mpc_announcement_days_tbl <- 
  read_rds(here("Outputs", "artifacts_model_data.rds")) |> 
  pluck(1)

us_mpc_announcement_days_tbl <- 
  read_rds(here("Outputs", "artifacts_model_data.rds")) |> 
  pluck(3)

# SA models ---------------------------------------------------------------
sa_decompose_ois_tbl <- decompose_ois(sa_mpc_announcement_days_tbl)

## Decomposed ------------------------------------------------------------
sa_differential_decompose_models_tbl <- 
  sa_decompose_ois_tbl |> 
  spot_component_models(data = sa_mpc_announcement_days_tbl) 

## Undecomposed ----------------------------------------------------------
sa_spot_undecomposed_models_tbl <- 
  spot_undecomposed_models(sa_decompose_ois_tbl, data = sa_mpc_announcement_days_tbl)

# US models ---------------------------------------------------------------
## Decomposed ------------------------------------------------------------
us_decompose_ois_tbl <- 
  decompose_ois(
    surprises = c(
      "target_models"                   = "jaracinski_ffr",
      "forward_guidance_models"         = "jaracinski_fg",
      "central_bank_information_models" = "jaracinski_cbi",
      "lsap_models"                     = "jaracinski_lsap"
    ),
    us_mpc_announcement_days_tbl)

us_differential_decompose_models_tbl <- 
  us_decompose_ois_tbl |> 
  spot_component_models(data = us_mpc_announcement_days_tbl) 
  

us_spot_undecomposed_models_tbl <- 
  spot_undecomposed_models(us_decompose_ois_tbl, data = us_mpc_announcement_days_tbl)

# Export ---------------------------------------------------------------
artifacts_differntial_models <- list (
  sa_differential_decompose_models_tbl = sa_differential_decompose_models_tbl,
  sa_spot_undecomposed_models_tbl = sa_spot_undecomposed_models_tbl,
  us_differential_decompose_models_tbl = us_differential_decompose_models_tbl,
  us_spot_undecomposed_models_tbl = us_spot_undecomposed_models_tbl )

write_rds(artifacts_differntial_models, file = here("Outputs", "artifacts_differntial_models.rds"))


