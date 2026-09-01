# Description
# interest rate differential - xolani 01 September 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
us_policy_rate_tbl <- 
  read_excel(here("Data", "us_policy_rate.xlsx"), sheet = 2) |> 
  rename(
    date = 1,
    polic_rate = 2
  ) |> 
  mutate(date = as.Date(date))


sa_policy_rate_tbl <- 
  read_excel(here("Data", "daily_repo_rate.xlsx"), skip = 4) |> 
  rename(date = 1, policy_rate = 2) |>
  select(1:2) |> 
  mutate(date = as.Date(date)) |> 
  drop_na(policy_rate) |> 
  filter(!policy_rate == "CLOSED") |>  # only for trading days
  mutate(policy_rate = as.numeric(policy_rate))


# Differential --------------------------------------------------------
rate_differential_tbl <- 
  sa_policy_rate_tbl |> 
  inner_join(us_policy_rate_tbl, by = "date") |> 
  rename(sa_policy_rate = policy_rate, us_policy_rate = polic_rate) |> 
  mutate(differential = sa_policy_rate - us_policy_rate)

# Export ---------------------------------------------------------------
artifacts_rate_differential <- list (
  rate_differential_tbl = rate_differential_tbl
)

write_rds(artifacts_rate_differential, file = here("Outputs", "artifacts_rate_differential.rds"))


