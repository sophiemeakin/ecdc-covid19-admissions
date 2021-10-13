
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
