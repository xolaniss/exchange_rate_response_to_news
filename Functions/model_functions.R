robust_model <-
function(model) {
    coeftest(
      model, vcov = NeweyWest(model)
    ) |> 
      tidy()
  }
news_models <-
function(data,
           surprises = c(
             "target_models"                   = "target",
             "forward_guidance_models"         = "forward_guidance",
             "central_bank_information_models" = "central_bank_information",
             "country_risk_models"             = "country_risk"
           ),
           predictors = c(
             "change_ln_spot_models" = "change_ln_spot",
             "change_ois_2y_models" = "change_ois_2y",
             "change_ois_5y_models" = "change_ois_5y",
             "change_ois_10y_models" = "change_ois_10y",
             "change_forward_2y_models" = "change_forward_2y",
             "change_forward_5y_models" = "change_forward_5y",
             "change_forward_10y_models" = "change_forward_10y"
           )) {
    surprises |> 
      purrr::map(function(surprise) {
        predictors |> 
          purrr::map(~ lm(data = data, formula = reformulate(surprise, .x))) |> 
          purrr::map(robust_model) |> 
          bind_rows(.id = "model")
      }) |> 
      bind_rows(.id = "surprise_type")
  }
