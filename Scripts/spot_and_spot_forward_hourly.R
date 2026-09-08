# Description
# Spot and spot forward data - 1 year August 2026 - hourly
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
names_vec <- excel_sheets(here("Data", "spot_data.xlsx"))
spot_data <- 
  names_vec |> 
  map(~read_excel(here("Data", "spot_data.xlsx"), sheet = .x, skip =1)) |> 
  set_names(names_vec)
  

# Cleaning -----------------------------------------------------------------
zarusd_spot_tbl <- 
  spot_data |> 
  pluck(1) |> 
  rename(date = "ZAR=...1", spot = "ZAR=...2") |> 
  arrange(date) 


zarusd_spot_forwards_tbl <- 
  spot_data |> 
  pluck(2)

pairs <- list(
  fwd1m = c("ZAR1MV=...1", "ZAR1MV=...2"),
  fwd2m = c("ZAR2MV=...4", "ZAR2MV=...5"),
  fwd3m = c("ZAR3MV=...7", "ZAR3MV=...8"),
  fwd6m = c("ZAR6MV=...10", "ZAR6MV=...11"),
  fwd1y = c("ZAR1YV=...13", "ZAR1YV=...14")
)

series_list <- imap(pairs, function(cols, nm) {
  zarusd_spot_forwards_tbl |>
    select(date = all_of(cols[1]), value = all_of(cols[2])) |>
    filter(!is.na(date), !is.na(value)) |>
    distinct(date, .keep_all = TRUE) |>
    rename(!!nm := value)
})

zarusd_common_tbl <- reduce(series_list, inner_join, by = "date") |>
  arrange(date) |> 
  left_join(zarusd_spot_tbl, by = "date") |> 
  drop_na() |> 
  relocate(spot, .before = "fwd1m")

# need to limit hours to business hours

# Export ---------------------------------------------------------------
artifacts_spot_and_spot_forwards_hourly <- list ( 
  zarusd_common_tbl = zarusd_common_tbl
)

write_rds(artifacts_spot_and_spot_forwards_hourly, file = here("Outputs", "artifacts_spot_spot_forwards_hourly.rds"))


