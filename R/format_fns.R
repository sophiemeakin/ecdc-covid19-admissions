

# Format data for convolution model ---------------------------------------

format_conv_data <- function(obs_dat, forecast_dat, cut_date, fdate) {
  
  # obs_dat <- dat %>% filter(id %in% fcast_ids$id[which(fcast_ids$trunc == 7)])
  # forecast_dat <- forecast_samples %>% filter(id %in% fcast_ids$id[which(fcast_ids$trunc == 7)])
  # cut_date <- "2021-11-13"
  # fdate <- "2021-11-20"
  
  # Observed cases and admissions
  out_obs <- obs_dat %>%
    filter(date >= as.Date(cut_date) - 12*7,
           date <= as.Date(cut_date)) %>%
    select(region = id, date, primary = cases, secondary = adm)
  
  # Observed "future" cases, used in forecast
  forecast_obs <- obs_dat %>%
    filter(date > as.Date(cut_date),
           date <= as.Date(fdate)) %>%
    select(region = id, date, cases)
  out_forecast_obs <- purrr::map_df(.x = 1:1000,
                                    .f = ~ {
                                      out <- forecast_obs %>%
                                        dplyr::mutate(sample = as.integer(.x))
                                      return(out)
                                      }) %>%
    dplyr::bind_rows()
  
  # Forecast future cases
  out_forecast_fut <- forecast_dat %>%
    select(region = id, date, sample, cases)
  
  # Combine forecast data
  out_forecast <- out_forecast_obs %>%
    bind_rows(out_forecast_fut)
  
  return(list(cc_obs = out_obs,
              cc_forecast = out_forecast))
  
}



# Format forecasts --------------------------------------------------------

format_forecast = function(forecast_summary, file_name, file_path) {
  
  out_int <- forecast_summary %>%
    ungroup() %>%
    mutate(type = "quantile",
           forecast_date = forecast_from + 1,
           target = paste0(horizon, " wk ahead inc hosp")) %>%
    select(forecast_date,
           target,
           target_end_date = date_horizon,
           location = id,
           type,
           quantile,
           value)
  
  out <- out_int %>%
    bind_rows(out_int %>%
                filter(quantile == 0.5) %>%
                mutate(type = "point", quantile = NA))
  
  message("Saving forecast...")
  write_csv(out, file = here::here(file_path, file_name))
  
}
