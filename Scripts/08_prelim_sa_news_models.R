# Description
# preliminary models

# Preliminaries -----------------------------------------------------------
library(here)
options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
source(here("Functions", "model_functions.R"))

# Import -------------------------------------------------------------
model_data <- read_rds(here("Outputs", "artifacts_model_data.rds"))


# MPC models  -----------------------------------------------------------------
mpc_announcement_days_tbl <- model_data |>  pluck(1)
mpc_surprises_models <- news_models(mpc_announcement_days_tbl)  |> 
  mutate(across(-c(surprise_type, model, term), ~ round(.x, 9))) |> 
  filter(!term == "(Intercept)")
mpc_surprises_models |>  print(n = 100)

mpc_surprises_models |> 
  filter(term != "(Intercept)") |> 
  mutate(
    surprise_type = str_remove(surprise_type, "_models$"),
    model         = str_remove(model, "_models$"),
    var_type = case_when(
      str_detect(model, "ois")     ~ "Interest rate differential",
      str_detect(model, "forward") ~ "Forward rate",
      TRUE                          ~ "Spot rate"
    ),
    tenor = case_when(
      str_detect(model, "2y")  ~ "2Y",
      str_detect(model, "5y")  ~ "5Y",
      str_detect(model, "10y") ~ "10Y",
      TRUE                      ~ "Spot"
    ),
    tenor     = fct_relevel(tenor, "Spot", "2Y", "5Y", "10Y"),
    ci_low    = estimate - 1.96 * std.error,
    ci_high   = estimate + 1.96 * std.error,
    sig_level = case_when(
      p.value < 0.05 ~ "p < 0.05",
      p.value < 0.10 ~ "p < 0.10",
      TRUE           ~ "n.s."
    ),
    sig_level = fct_relevel(sig_level, "p < 0.05", "p < 0.10", "n.s.")
  ) |> 
  ggplot(aes(x = tenor, y = estimate, color = sig_level, group = surprise_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high, shape = surprise_type),
                  position = position_dodge(width = 0.5), size = 0.6) +
  scale_color_manual(values = c("p < 0.05" = "#d62728", "p < 0.10" = "#ff7f0e", "n.s." = "grey60")) +
  facet_wrap(~ var_type, scales = "free_y", ncol = 3) +
  labs(x = "Tenor", y = "Coefficient estimate",
       color = "Significance", shape = "Surprise type",
       title = "Term-structure of market responses to MPC surprise factors")

## Residual models ---------------------------------------------------------
residuals_tbl <- 
  news_model_residuals(mpc_announcement_days_tbl)

residual_models_tbl <- residual_models(residuals_tbl)


## Residual plots ----------------------------------------------------------
spot_resids_tbl <- 
  residuals_tbl |> 
  dplyr::filter(str_detect(model, "ln_spot")) |> 
  dplyr::select(surprise_type, date, resid_spot = .resid)

ois_resids_tbl <-  
  residuals_tbl  |> 
  dplyr::filter(str_detect(model, "ois")) |> 
  dplyr::select(surprise_type, date, ois_model = model, resid_ois = .resid) |> 
  dplyr::mutate(ois_model = str_remove(ois_model, "change_|_models"))

scatter_plot_data_tbl <- 
  dplyr::inner_join(spot_resids, ois_resids, by = c("surprise_type", "date")) |> 
  dplyr::mutate(
    surprise_type = str_remove(surprise_type, "_models"),
    ois_model     = toupper(str_replace(ois_model, "ois_", "OIS "))
  ) |> 
  dplyr::mutate(
    ois_model     = str_remove(ois_model, "_MODELS") |> str_replace("OIS ", "OIS ") |> 
      factor(levels = c("OIS 2Y", "OIS 5Y", "OIS 10Y")),
    surprise_type = str_replace_all(surprise_type, "_", " ") |> str_to_title()
  )

ggplot(scatter_plot_data_tbl, aes(x = resid_ois, y = resid_spot)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, color = "steelblue") +
  facet_grid(surprise_type ~ ois_model, scales = "free_x",
             labeller = labeller(surprise_type = label_wrap_gen(15))) +
  labs(x = "OIS residual", y = "Spot residual",
       title = "Residual-on-residual: spot vs OIS after partialling out surprise factor") +
  theme(strip.text.y = element_text(size = 7)) +
  theme_minimal()


# News models -------------------------------------------------------------
news_announcement_days_tbl <- model_data |>  pluck(2) |> 
  janitor::clean_names()

data_releases_models_tbl <- 
  news_models(
  data = news_announcement_days_tbl,
  surprises = c(
    "cpi_models" = "cpi_yo_y",
    "gdp_models" = "gdp_yo_y",
    "south_africa_unemployment_models" = "south_africa_unemployment"
  ),
  predictors = c(
    "change_ln_spot_models" = "change_ln_spot",
    "change_ois_2y_models" = "change_ois_2y",
    "change_ois_5y_models" = "change_ois_5y",
    "change_ois_10y_models" = "change_ois_10y",
    "change_forward_2y_models" = "change_forward_2y",
    "change_forward_5y_models" = "change_forward_5y",
    "change_forward_10y_models" = "change_forward_10y"
  )
)

data_releases_models_tbl |> 
  mutate(across(-c(surprise_type, model, term), ~ round(.x, 9))) |> 
  filter(!term == "(Intercept)") |> 
  print(n = 200)

# Export ---------------------------------------------------------------
artifacts_sa_news_models <- list (
  mpc_surprises_models = mpc_surprises_models,
  data_releases_models_tbl = data_releases_models_tbl
)

write_rds(artifacts_sa_news_models, file = here("Outputs", "artifacts_sa_news_models.rds"))


