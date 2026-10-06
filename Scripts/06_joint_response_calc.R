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
    ln_spot        = log(spot),
    # Daily log return of spot
    change_ln_spot = (ln_spot - lag(ln_spot)),
    
    # Daily change in SA-US OIS interest rate differential (pp)
    change_ois_2y  = (sa_ois_2y  - us_ois_2y)  - lag(sa_ois_2y  - us_ois_2y) ,
    change_ois_5y  = (sa_ois_5y  - us_ois_5y)  - lag(sa_ois_5y  - us_ois_5y) ,
    change_ois_10y = (sa_ois_10y - us_ois_10y) - lag(sa_ois_10y - us_ois_10y) ,
    
    # Daily log return of observed outright forward exchange rates
    change_forward_2y = (log(forward_2Y) - lag(log(forward_2Y))),
    change_forward_5y = (log(forward_5Y) - lag(log(forward_5Y))) ,
    
    # 10Y outright forward not available; CIP approximation:
    # Δlog(F^10Y) ≈ Δlog(S) + 10 × Δ(r_SA - r_US)
    change_forward_10y = change_ln_spot + 10 * change_ois_10y
  ) |> 
  drop_na()




# descriptive table -------------------------------------------------------
descriptive_statistics_tbl <- 
  joint_response_data_tbl |> 
  select(date, starts_with("change")) |> 
  pivot_longer(-date, names_to = "series", values_to = "rate") |> 
  group_by(series) |> 
  summarise(
    "Mean" = mean(rate, na.rm = TRUE),
    "Median" = median(rate, na.rm = TRUE),
    "SD" = sd(rate, na.rm = TRUE),
    "Min" = min(rate, na.rm = TRUE),
    "Max" = max(rate, na.rm = TRUE),
    "Observations" = n()
  ) 




# graphing ----------------------------------------------------------------
joint_response_gg <- 
  joint_response_data_tbl |> 
  select(date, starts_with("change")) |> 
  pivot_longer(-date, names_to = "series", values_to = "rate") |> 
  ggplot(aes(x = date, y = rate, color = series)) +
  geom_line() +
  facet_wrap(~series, scales = "free_y", ncol = 2) +
  theme_linedraw() +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay",7), labels = scales::label_wrap(20))




# Export ---------------------------------------------------------------
artifacts_joint_response_data <- list (
  joint_response_data_tbl = joint_response_data_tbl,
  joint_response_data_gg = joint_response_gg,
  descriptive_statistics_tbl = descriptive_statistics_tbl
)

write_rds(artifacts_joint_response_data, file = here("Outputs", "artifacts_joint_response_data.rds"))


