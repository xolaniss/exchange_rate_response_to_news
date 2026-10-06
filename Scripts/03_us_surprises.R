# Description
# US Surprises - Sep 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import ------------------------------------------------------------------
jaracinski_surprises_tbl <- 
  read_csv(here("Data", "U1s.csv")) |> 
  rename(
    "Date" = Time,
    "Jaracinski - FFR" = u1,
    "Jaracinski - FG" = u2,
    "Jaracinski - LSAP" = u3,
    "Jaracinski - CBI"= u4
  ) |> 
  mutate(Date = as.Date(Date)) |> 
  # remove duplicate dates
  distinct(Date, .keep_all = TRUE) # removed entries with multiple times. sample when from 297 to 291



swanson_surprises_tbl <- 
  read_excel(here("Data", "factors2023.xlsx")) |> 
  mutate(
    Date = as.Date(paste0(year,"-", month, "-", day))
  ) |> 
  relocate(
    Date,
    .before = year
  ) |> 
  dplyr::select(
    -c(year, month, day, `-LSAP`)
  ) |> 
  rename(
    "Swanson - FFR" = `FFR`,
    "Swanson - FG" = `FG`,
    "Swanson - LSAP" = `LSAP`
  )


# Combining ---------------------------------------------------------------
us_surprises_tbl <- 
  left_join(
    jaracinski_surprises_tbl,
    swanson_surprises_tbl,
    by = c("Date" = "Date")
  ) |> 
  distinct(Date, .keep_all = TRUE)  # removed entries with multiple times. sample when from 297 to 291


us_surprises_gg <- 
  us_surprises_tbl |> 
pivot_longer(-Date, names_to = "variable", values_to = "surprise") |>
  ggplot(aes(x = Date, y = surprise, col = variable)) + # Change the variable names
  geom_line() +
  labs(
    # title = "SA Market-Based MPS",
    x = "",
    y = "Surprise",
    title = "US Market-Based MPS",
    x = "Date",
    y = " "
  ) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  theme_linedraw() +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  scale_color_manual(values = pnw_palette("Bay",7), labels = scales::label_wrap(20))


# Export -------------------------------------------------------------------
artifacts_us_surprises <- list (
  us_surprises_tbl = us_surprises_tbl,
  us_surprises_gg = us_surprises_gg
)

write_rds(artifacts_us_surprises, file = here("Outputs", "artifacts_us_surprises.rds"))



