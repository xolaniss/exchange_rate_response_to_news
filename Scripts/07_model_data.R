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
  pluck(1) |> 
  select(date, change_ln_spot, change_ois_2y, change_ois_5y, change_forward_2y, change_forward_5y)
sa_market_surprises_tbl <- 
  read_rds(here("Outputs", "artifacts_market_based_surprises.rds")) |> 
  pluck(1) |> 
  janitor::clean_names()
eme_surprise_tbl <- read_rds(here("Outputs", "artifacts_eme_surprises.rds")) |> 
  pluck(1) |> 
  # Keep South African releases only; other EMs share generic ev ent
  # labels (e.g. "CPI YoY") that would otherwise collide when pivoting
  rename(date = release_date) |>
  mutate(date = as.Date(date)) |>
  pivot_wider(id_cols = date, names_from = event, values_from = surprise) |> 
  select(-starts_with("SARB"))

  eme_surprise_tbl |> 
    skim()
# Graphs -------------------------------------
## joint response ----
joint_response_data_tbl |> 
  pivot_longer(-date, names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal()

## sa market surprises ----
sa_market_surprises_tbl |> 
  pivot_longer(-date, names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal()


## eme surprises ----
eme_surprise_tbl |> 
  pivot_longer(-date, names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal()

# Reducing to announcement days -------------------------

## MPC announcement days ----
mpc_announcement_days_tbl <- 
  sa_market_surprises_tbl |> 
  inner_join(joint_response_data_tbl, by = "date")
  # inner_join(eme_surprise_tbl, by = "date") we don't have releases on announcement

mpc_announcement_days_tbl |>
  pivot_longer(-date, names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal()
  

## news announcement days ----
news_announcement_days_tbl <- 
  eme_surprise_tbl |> 
  inner_join(joint_response_data_tbl, by = "date")
  news_announcement_days_tbl

news_announcement_days_tbl |> 
  pivot_longer(-date, names_to = "variable", values_to = "value") |> 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal()
  
  
# Export ---------------------------------------------------------------
artifacts_model_data <- list (
 mpc_announcement_days_tbl = mpc_announcement_days_tbl,
 news_announcement_days_tbl = news_announcement_days_tbl
)

write_rds(artifacts_model_data , file = here("Outputs", "artifacts_model_data.rds"))


