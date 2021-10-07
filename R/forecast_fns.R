# ARIMA with xreg wrapper -------------------------------------------------

## Based on EpiSoon::forecast_model, with some modifications
## Used internally in timeseries_samples

## Returns ARIMA + xreg forecast samples

forecast_model_xreg <- function(y = NULL, samples = NULL, horizon = NULL,
                                model_params = NULL, forecast_params = NULL,
                                ...) {
  
  # convert to timeseries object
  timeseries <- stats::ts(y)
  
  # fit and forecast
  fit <- suppressMessages(
    suppressWarnings(
      do.call(forecast::auto.arima,
              c(list(y = timeseries), model_params))
    )
  )
  prediction <- do.call(forecast::forecast,
                        c(list(object = fit, h = horizon), forecast_params))
  
  # extract samples and tidy format
  sample_from_model <- prediction
  
  if (samples == 1) {
    sample_from_model <- data.frame(t(as.data.frame(sample_from_model$mean)))
    rownames(sample_from_model) <- NULL
  } else {
    mean <- as.numeric(prediction$mean)
    upper <- prediction$upper[, ncol(prediction$upper)]
    lower <-  prediction$lower[, ncol(prediction$lower)]
    sd <- (upper - lower) / 3.92
    sample_from_model <- purrr::map2(mean, sd,
                                     ~ rnorm(samples, mean = .x,  sd = .y))
    
    sample_from_model <- suppressMessages(dplyr::bind_cols(sample_from_model))
  }
  
  return(sample_from_model)
  
}


# Fit time series model(s) ------------------------------------------------
## Based on `forecast` and `forecastHybrid` wrappers in `EpiSoon`
## Fits models:
##  Autoregressive time series: ARIMA (models = "a"), ETS ("e"), TBATS ("t"), naive ("z")
##  Dynamic regression: ARIMA + xreg (models = "a", xreg = c("predictor1", "predictor2"))

# Parameters:
#   data (data.frame): columns "id", "date", yvar, xvars
#   yvar (string): name of column to forecast
#   xvars (string): name(s) of predictors (used in ARIMA regression model only)
#   horizon (integer): length of horizon (days) for forecasting. Default value 14
#   samples (integer): number of samples to take for timeseries model(s). Default value 1000
#   train_from (date or string "YYYY-MM-DD"): date to begin training window from
#   forecast_from (date or string "YYYY-MM-DD"): date to begin forecasting from
#   models (string): model(s) to forecast with; if multiple models are given, an mean-weighted ensemble forecast is returned

## Returns forecast samples

timeseries_samples <- function(data = NULL, yvar = NULL, xvars = NULL,
                               horizon = 14, samples = 1000, train_from = NULL, forecast_from = NULL, models = NULL){
  
  data <- data %>%
    dplyr::mutate(forecast = date > forecast_from) %>%
    dplyr::filter(date > as.Date(train_from),
                  date <= as.Date(forecast_from) + horizon) 
  
  
  if(stringr::str_length(models) > 1){
    
    # time series ensemble model
    forecast <- data %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(~ EpiSoon::forecastHybrid_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                          samples = samples, 
                                                          horizon = horizon,
                                                          model_params = list(models = models, weights = "equal"),
                                                          forecast_params = list(PI.combination = "mean"))) %>%
      dplyr::mutate(sample = rep(1:samples)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
      dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                    value = ifelse(value < 0, 0, value),
                    forecast_from = forecast_from)
    
  } else if (models == "a") {
    
    if(is.null(xvars)){
      
      # ARIMA model
      forecast <- data %>%
        dplyr::group_by(id) %>%
        dplyr::group_modify(~ EpiSoon::forecast_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                      samples = samples, 
                                                      horizon = horizon,
                                                      model = forecast::auto.arima)) %>%
        dplyr::mutate(sample = rep(1:samples)) %>%
        tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
        dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                      value = ifelse(value < 0, 0, value),
                      forecast_from = forecast_from)
      
    } else {
      
      # ARIMA regression model
      forecast <- data %>%
        dplyr::group_by(id) %>%
        dplyr::group_modify( ~ forecast_model_xreg(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                   samples = samples, 
                                                   horizon = horizon,
                                                   model_params = list(
                                                     xreg = as.matrix(.x %>%
                                                                        filter(!forecast) %>%
                                                                        select(xvars))
                                                   ),
                                                   forecast_params = list(
                                                     xreg = as.matrix(.x %>%
                                                                        filter(forecast) %>%
                                                                        select(xvars))
                                                   )
        )) %>%
        dplyr::mutate(sample = rep(1:samples)) %>%
        tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
        dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                      value = ifelse(value < 0, 0, value),
                      forecast_from = forecast_from)
      
    }
    
  } else if(models == "e") {
    
    # ETS model
    forecast <- data %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(~ EpiSoon::forecast_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                    samples = samples, 
                                                    horizon = horizon,
                                                    model = forecast::ets)) %>%
      dplyr::mutate(sample = rep(1:samples)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
      dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                    value = ifelse(value < 0, 0, value),
                    forecast_from = forecast_from)
    
  } else if(models == "t") {
    
    # TBATS model
    forecast <- data %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(~ EpiSoon::forecast_model(y = .x %>% filter(!forecast) %>% pull(yvar),
                                                    samples = samples, 
                                                    horizon = horizon,
                                                    model = forecast::tbats)) %>%
      dplyr::mutate(sample = rep(1:samples)) %>%
      tidyr::pivot_longer(cols = starts_with("..."), names_to = "horizon") %>%
      dplyr::mutate(horizon = as.numeric(str_remove_all(horizon, "...")) - 1,
                    value = ifelse(value < 0, 0, value),
                    forecast_from = forecast_from)
    
  } else {
    
    message("Check models used.")
    forecast <- NA
    
  }
  
  return(forecast)
  
}


# Summarise forecast samples ----------------------------------------------

# Returns data.frame with quantile forecasts
# by model, id, horizon and forecast date (forecast_from)

# Parameters:
# samples (data.frame): with columns "model", "id", "horizon", "forecast_from", "value" (e.g. output from timeseries_samples)
# quantiles : vector of quantiles to output. Default value c(0.05, 0.25, 0.5, 0.75, 0.95)

forecast_summary <- function(samples = NULL, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)){
  
  out <- samples %>%
    group_by(model, id, horizon, forecast_from) %>%
    dplyr::group_modify( ~ quantile(.x$value, probs = quantiles, na.rm = T) %>%
                           tibble::enframe(name = "quantile", value = "value")) %>%
    dplyr::mutate(date_horizon = forecast_from + horizon,
                  value = round(value),
                  quantile = as.numeric(str_remove(quantile, "%"))/100,
                  quantile_range = abs(1 - 2 * quantile) * 100,
                  quantile_boundary = ifelse(quantile <= 0.5, "lower", "upper"),
                  quantile_label = paste0(quantile_boundary, "_", quantile_range)) %>%
    dplyr::select(model, forecast_from, id, horizon, date_horizon, quantile, quantile_label, value)
  
  out <- out %>%
    dplyr::bind_rows(out %>%
                       dplyr::filter(quantile_label == "lower_0") %>%
                       dplyr::mutate(quantile_label = "upper_0"))
  
  return(out)
  
}
