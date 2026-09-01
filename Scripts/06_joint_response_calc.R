# Description
# Two approaches to calculating joint response to news - Xolani 01 September 

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
differential_tbl <- read_rds(here("Outputs", "artifacts_rate_differential.rds")) |> 
  pluck(1)

spot_forward_tbl <- read_rds(here("Outputs", "artifacts_spot_and_spot_forwards_daily.rds")) |> 
  pluck(1)


# Combine ------------------------------
combined_tbl <- 
  spot_forward_tbl |> 
  inner_join(differential_tbl, by = "date") 
  

# First approach - uncovered interest rate parity -------------------------
combined_tbl |> 
  mutate(
    ln_spot  = log(spot),
    change_differential = differential - lag(differential),
    across(starts_with("forward"), ~ .x - lag(.x, 1), .names = "change_{.col}"),
    change_spot = change_differential + change_forward_1Y, # assuming zero risk premium,
    synthetic_forward_rate = spot *((1 + us_policy_rate)/(1 + sa_policy_rate))
  ) |> 
  glimpse()

  
  


# Transformations --------------------------------------------------------


# EDA ---------------------------------------------------------------


# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


