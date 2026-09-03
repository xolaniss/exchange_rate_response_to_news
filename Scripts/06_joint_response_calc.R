# Description
# joint response to news variables - Xolani 01 September 

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
ois_tbl <- read_rds(here("Outputs", "artifacts_interest_rate.rds")) |> 
  pluck(1)

spot_forward_tbl <- read_rds(here("Outputs", "artifacts_spot_and_spot_forwards_daily.rds")) |> 
  pluck(1)


# Combine ------------------------------
combined_tbl <- 
  spot_forward_tbl |> 
  inner_join(ois_tbl, by = "date") 
  

# model data -------------------------
joint_response_data_tbl <- 
  combined_tbl |> 
  mutate(
    ln_spot = log(spot),
    change_ln_spot = ln_spot - lag(ln_spot, 1),
    change_ois_2y = 2*(sa_ois_2y - us_ois_2y),
    change_ois_5y = 5*(sa_ois_5y - us_ois_5y),
    change_forward_2y = change_ln_spot - change_ois_2y,
    change_forward_5y = change_ln_spot - change_ois_5y
  ) |> 
  drop_na()


# Export ---------------------------------------------------------------
artifacts_joint_response_data <- list (
  joint_response_data_tbl = joint_response_data_tbl 
)

write_rds(artifacts_joint_response_data, file = here("Outputs", "artifacts_joint_response_data.rds"))


