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
          dplyr::bind_rows(.id = "model")
      }) |> 
      dplyr::bind_rows(.id = "surprise_type")
  }

news_model_residuals <- 
  function(data,
           surprises = c(
             "target_models"                   = "target",
             "forward_guidance_models"         = "forward_guidance",
             "central_bank_information_models" = "central_bank_information",
             "country_risk_models"             = "country_risk"
           ),
           predictors = c(
             "change_ln_spot_models"     = "change_ln_spot",
             "change_ois_2y_models"      = "change_ois_2y",
             "change_ois_5y_models"      = "change_ois_5y",
             "change_ois_10y_models"     = "change_ois_10y",
             "change_forward_2y_models"  = "change_forward_2y",
             "change_forward_5y_models"  = "change_forward_5y",
             "change_forward_10y_models" = "change_forward_10y"
           )) {
    surprises |> 
      purrr::map(function(surprise) {
        predictors |> 
          purrr::map(function(predictor) {
            lm(data = data, formula = reformulate(surprise, predictor)) |> 
              broom::augment(data = data) |> 
              dplyr::select(dplyr::any_of("date"), response = all_of(predictor), .resid)
          }) |> 
          dplyr::bind_rows(.id = "model")
      }) |> 
      dplyr::bind_rows(.id = "surprise_type")
  }

residual_models <- 
  function(residuals_data,
           response_pattern  = "ln_spot",
           predictor_pattern = "ois") {
    
    residuals_data |> 
      split(~surprise_type) |> 
      purrr::map(function(group_data) {
        
        # response: spot residuals for this surprise type
        response_resid <- group_data |> 
          dplyr::filter(stringr::str_detect(model, response_pattern)) |> 
          dplyr::select(date, resid_response = .resid)
        
        # predictors: one residual series per OIS tenor
        predictor_models <- group_data |> 
          dplyr::filter(stringr::str_detect(model, predictor_pattern)) |> 
          dplyr::distinct(model) |> 
          dplyr::pull(model) |> 
          purrr::set_names()
        
        predictor_models |> 
          purrr::map(function(pred_model) {
            predictor_resid <- group_data |> 
              dplyr::filter(model == pred_model) |> 
              dplyr::select(date, resid_predictor = .resid)
            
            dplyr::inner_join(response_resid, predictor_resid, by = "date") |> 
              lm(resid_response ~ resid_predictor, data = _) |> 
              robust_model()
          }) |> 
          dplyr::bind_rows(.id = "predictor_model")
      }) |> 
      dplyr::bind_rows(.id = "surprise_type")
  }
