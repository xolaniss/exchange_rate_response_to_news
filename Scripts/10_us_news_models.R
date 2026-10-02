# Description
# preliminary models - XS Sep 2026

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
us_mpc_announcement_days_tbl <- model_data |>  pluck(3)
us_mpc_surprises_models <- news_models(us_mpc_announcement_days_tbl, 
                                       surprises = c(
                                         "target_models"                   = "jaracinski_ffr",
                                         "forward_guidance_models"         = "jaracinski_fg",
                                         "central_bank_information_models" = "jaracinski_cbi",
                                         "lsap_models"                     = "jaracinski_lsap"
                                       ))  |> 
  mutate(across(-c(surprise_type, model, term), ~ round(.x, 9))) |> 
  filter(!term == "(Intercept)")

us_mpc_surprises_models |>  print(n = 100)



us_mpc_surprises_models  |> 
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
    sig_level = fct_relevel(sig_level, "p < 0.05", "p < 0.10", "n.s."),
    var_type = fct_relevel(var_type, "Spot rate", "Forward rate", "Interest rate differential")
  ) |> 
  ggplot(aes(x = tenor, y = estimate, color = sig_level, group = surprise_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high, shape = surprise_type),
                  position = position_dodge(width = 0.5), size = 0.6) +
  scale_color_manual(values = c("p < 0.05" = "#d62728", "p < 0.10" = "#ff7f0e", "n.s." = "grey60")) +
  facet_wrap(~ var_type, scales = "free", ncol = 3) +
  labs(x = "Tenor", y = "Coefficient estimate",
       color = "Significance", shape = "Surprise type",
       title = "Term-structure of market responses to US MP surprise factors") +
  theme_linedraw()



# Export ---------------------------------------------------------------
artifacts_us_news_models <- list (
  us_mpc_surprises_models = us_mpc_surprises_models
)

write_rds(artifacts_us_news_models, file = here("Outputs", "artifacts_us_news_models.rds"))


