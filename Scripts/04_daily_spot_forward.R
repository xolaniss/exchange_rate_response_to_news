# Description

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
names_vec <- excel_sheets(here("Data", "X_Daily.xlsx"))

spot <- names_vec |> 
  set_names() |> 
  map(~read_excel(here("Data", "X_Daily.xlsx"), sheet = .x))

# ZARUSD Spot -----------------------------------------------------------------
spot_tbl <- 
  spot |> 
  pluck(1) |> 
  select(2, 3) |> 
  slice(-c(1:2)) |> 
  rename(date = 1, spot = 2) |> 
  mutate(
    spot = as.numeric(spot),
    spot = round(spot, 2)
    ) |> 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"))

# Spot forward --------------------------------------------------------
spot_forward_tbl <- 
  spot |> 
  pluck(2) |> 
  select(2:6) |> 
  slice(-c(1:2)) |> 
  rename(
    date = 1,
    ZAR1M = 2,
    ZAR3M = 3,
    ZAR6M = 4,
    ZAR1Y = 5
  ) |> 
  mutate(across(everything(), as.numeric)) |> 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) 

# Outright forward rates ----------------------------------------------
# Forward = spot + swap points, points quoted per 10,000 (4 decimal pips)
spot_and_forward_tbl <- 
  spot_tbl |> 
  inner_join(spot_forward_tbl, by = "date") |> 
  mutate(
    forward_1M = spot + ZAR1M / 10000,
    forward_3M = spot + ZAR3M / 10000,
    forward_6M = spot + ZAR6M / 10000,
    forward_1Y = spot + ZAR1Y / 10000
  ) |> 
  select(date, spot, starts_with("forward"))


# Plots -------------------- 

spot_and_forward_tbl |> # long term
  select(date, spot, forward_1M, forward_3M, forward_6M, forward_1Y) |> 
  pivot_longer(-date, names_to = "series", values_to = "rate") |> 
  ggplot(aes(x = date, y = rate, color = series)) +
  geom_line()


spot_and_forward_tbl |> # recent curve
  filter(date >= as.Date("2025-01-01")) |> 
  select(date, spot, forward_1M, forward_3M, forward_6M, forward_1Y) |> 
  pivot_longer(-date, names_to = "series", values_to = "rate") |> 
  ggplot(aes(x = date, y = rate, color = series)) +
  geom_line()

spot_and_forward_tbl |>  
  mutate(
    premium_1M = forward_1M - spot,
    premium_3M = forward_3M - spot,
    premium_6M = forward_6M - spot,
    premium_1Y = forward_1Y - spot
  ) |> 
  select(date, starts_with("premium")) |> 
  pivot_longer(-date, names_to = "tenor", values_to = "premium") |> 
  ggplot(aes(x = date, y = premium, color = tenor)) +
  geom_line()

# Export ---------------------------------------------------------------
artifacts_spot_and_spot_forwards <- list (
  spot_and_forward_tbl = spot_and_forward_tbl
)

write_rds(artifacts_spot_and_spot_forwards, file = here("Outputs", "artifacts_spot_and_spot_forwards.rds"))


