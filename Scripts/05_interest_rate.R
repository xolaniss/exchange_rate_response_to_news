# Description
# interest rate differential - xolani 01 September 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
us_ois_tbl <- 
  read_excel(here("Data", "us_ois.xlsx"))

date_cols <- us_ois_tbl |> select(where(~inherits(., "POSIXct"))) |> names()
value_cols <- us_ois_tbl |> select(where(is.numeric)) |> names()
list(date_cols = date_cols, value_cols = value_cols)
tenor_pairs <- setNames(as.list(date_cols), value_cols)

series_list <- imap(tenor_pairs, function(date_col, tenor) {
  us_ois_tbl |> 
    select(date = all_of(date_col), value = all_of(tenor)) |> 
    filter(!is.na(date), !is.na(value)) |> 
    distinct(date, .keep_all = TRUE) |> 
    rename(!!tenor := value)
})

us_ois_tbl <- reduce(series_list, inner_join, by = "date") |> 
  arrange(date) |> 
  mutate(date = as.Date(date)) |> 
  select(date, "1Y", "2Y", "5Y", "10Y") |> 
  rename(
    us_ois_1y = "1Y",
    us_ois_2y = "2Y",
    us_ois_5y = "5Y",
    us_ois_10y = "10Y"
  )


sa_ois_tbl <-
  read_excel(here("Data", "zaronia.xlsx"), skip = 1) |> 
  mutate(
    across(-1, ~ .x * 100),
    Dates = as.Date(Dates)) |> 
  janitor::clean_names() |> 
  rename(date = dates) |> 
  select(date, ends_with(c("12m", "2y", "5y", "10y"))) |> 
  select(-ends_with(c("12y", "22y", "15", "25y"))) |> 
  pivot_longer(-date, names_to = "names", values_to = "value") |> 
  mutate(
    names = str_replace_all(names, "sssp_ois_12m", "sa_ois_1y"),
    names = str_replace_all(names, "sssp_ois_", "sa_ois_"),
    names = str_replace_all(names, "ssmp_ois_", "sa_ois_")
  ) |> 
  pivot_wider(names_from = names, values_from = value)

  
# Combine -----------------------------------------------
combined_interest_tbl <- 
  sa_ois_tbl |> 
  inner_join(us_ois_tbl, by = "date")

# Export ---------------------------------------------------------------
artifacts_interest_rate <- list (
  combined_interest_tbl = combined_interest_tbl
)

write_rds(artifacts_interest_rate, file = here("Outputs", "artifacts_interest_rate.rds"))


