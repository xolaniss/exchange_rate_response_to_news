# Description
# surprises model - XS Sept 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
joint_response_data_tbl <- 
  read_rds(here("Outputs", "artifacts_joint_response_data.rds")) |> 
  pluck(1)
sa_market_surprises_tbl <- 
  read_rds(here("Outputs", "artifacts_market_based_surprises.rds")) |> 
  pluck(1) |> 
  janitor::clean_names()
eme_surprise_tbl <- read_rds(here("Outputs", "artifacts_eme_surprises.rds")) |> 
  pluck(1) |> 
  # Keep South African releases only; other EMs share generic event
  # labels (e.g. "CPI YoY") that would otherwise collide when pivoting
  filter(startsWith(ticker, "SA")) |> 
  rename(date = release_date) |> 
  mutate(date = as.Date(date)) |> 
  pivot_wider(id_cols = date, names_from = event, values_from = surprise) 

# Reducing to announcement days -------------------------
announcement_days_tbl <- 
  sa_market_surprises_tbl |> 
  inner_join(joint_response_data_tbl, by = "date")
  # inner_join(eme_surprise_tbl, by = "date") we don't have releases on announcement



# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


