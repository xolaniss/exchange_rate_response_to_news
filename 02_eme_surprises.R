# Description
# EME surprises data - August 2026
# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
eme_surprises_tbl <- 
  read_excel(here("Data", "EME_surprises_all.xlsx")) |> 
  janitor::clean_names() |> 
  mutate(release_date = lubridate::parse_date_time(release_date, orders = "m/d/Y"),
         estimation_date = lubridate::parse_date_time(estimation_date, orders = "m/d/Y")) 

# Transformations --------------------------------------------------------
## Come back ones its clear what to do with the data

# EDA ---------------------------------------------------------------
eme_surprises_tbl |> 
  group_by(event) |> 
  skim()

# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_eme_surprises <- list (
eme_surprises_tbl = eme_surprises_tbl
)

write_rds(artifacts_eme_surprises, file = here("Outputs", "artifacts_eme_surprises.rds"))


