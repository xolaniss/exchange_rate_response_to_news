# Description
# differential decomposition - XS Sep 2026

# Preliminaries -----------------------------------------------------------
library(here)

# Functions ---------------------------------------------------------------
source(here("packages.R"))
source(here("Functions", "fx_plot.R"))
source(here("Functions", "model_functions.R"))

# Import -------------------------------------------------------------
sa_mpc_announcement_days_tbl <- 
  read_rds(here("Outputs", "artifacts_model_data.rds")) |> 
  pluck(1)

us_mpc_announcement_days_tbl <- 
  read_rds(here("Outputs", "artifacts_model_data.rds")) |> 
  pluck(3)

# SA models ---------------------------------------------------------------
sa_decompose_ois_tbl <- decompose_ois(sa_mpc_announcement_days_tbl)

## Decomposed ------------------------------------------------------------
sa_differential_decompose_models_tbl <- 
  sa_decompose_ois_tbl |> 
  spot_component_models(data = sa_mpc_announcement_days_tbl) 

## Undecomposed ----------------------------------------------------------
sa_spot_undecomposed_models_tbl <- 
  spot_undecomposed_models(sa_decompose_ois_tbl, data = sa_mpc_announcement_days_tbl)

# US models ---------------------------------------------------------------
## Decomposed ------------------------------------------------------------
us_decompose_ois_tbl <- 
  decompose_ois(
    surprises = c(
      "target_models"                   = "jaracinski_ffr",
      "forward_guidance_models"         = "jaracinski_fg",
      "central_bank_information_models" = "jaracinski_cbi",
      "lsap_models"                     = "jaracinski_lsap"
    ),
    us_mpc_announcement_days_tbl)

us_differential_decompose_models_tbl <- 
  us_decompose_ois_tbl |> 
  spot_component_models(data = us_mpc_announcement_days_tbl) 
  

us_differential_decompose_models_tbl |> 
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
       title = "Residual model")


us_spot_undecomposed_models_tbl <- 
  spot_undecomposed_models(us_decompose_ois_tbl, data = us_mpc_announcement_days_tbl)


# Export ---------------------------------------------------------------
artifacts_differntial_models <- list (
  sa_differential_decompose_models_tbl = sa_differential_decompose_models_tbl,
  sa_spot_undecomposed_models_tbl = sa_spot_undecomposed_models_tbl,
  us_differential_decompose_models_tbl = us_differential_decompose_models_tbl,
  us_spot_undecomposed_models_tbl = us_spot_undecomposed_models_tbl )

write_rds(artifacts_differntial_models, file = here("Outputs", "artifacts_differntial_models.rds"))


