
# Plot hub ensemble case forecast -----------------------------------------

plot_ensemble <- function(dat_obs, dat_for, forecast_date, regions) {
  
  # Add observed data
  plot_obs <- dat_obs %>%
    filter(location %in% regions,
           week >= as.Date(forecast_date) - 8*7,
           week <= as.Date(forecast_date)) %>%
    mutate(week = week + 6)
  g <- plot_obs %>%
    ggplot(aes(x = week, y = cases)) +
    geom_line() +
    facet_wrap(. ~ location, scales = "free_y") +
    scale_x_date(breaks = seq.Date(from = forecast_date - 8*7,
                                   to = forecast_date + 4*7,
                                   by = "2 weeks"),
                 date_labels = "%d %b") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Week ending",
         y = "Week cases",
         title = forecast_date,
         col = "Model",
         fill = "Model") +
    theme_bw() +
    theme(legend.position = "top")
  
  # Add hub-ensemble case forecast
  plot_forecast <- dat_for %>%
    filter(location %in% regions,
           quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95)) %>%
    mutate(quantile = paste0("q", quantile)) %>%
    pivot_wider(id_cols = -c(quantile, value), names_from = quantile)
  out <- g + 
    geom_ribbon(data = plot_forecast,
                aes(x = target_end_date, y = q0.5, ymin = q0.05, ymax = q0.95),
                fill = "grey50", alpha = 0.4) +
    geom_ribbon(data = plot_forecast,
                aes(x = target_end_date, y = q0.5, ymin = q0.25, ymax = q0.75),
                fill = "grey50", alpha = 0.4) +
    geom_point(data = plot_forecast,
               aes(x = target_end_date, y = q0.5)) + 
    geom_line(data = plot_forecast,
              aes(x = target_end_date, y = q0.5))
  
  return(out)
  
}



# Plot model forecasts ----------------------------------------------------

plot_forecasts <- function(dat_obs, forecast_date, regions) {
  
  # Add observed data
  plot_obs <- dat_obs %>%
    filter(location %in% regions,
           week >= as.Date(forecast_date) - 8*7,
           week < as.Date(forecast_date)) %>%
    mutate(week = week + 6)
  g <- plot_obs %>%
    ggplot(aes(x = week, y = adm)) +
    geom_line() +
    facet_wrap(. ~ location, scales = "free_y") +
    scale_x_date(breaks = seq.Date(from = forecast_date - 8*7,
                                   to = forecast_date + 4*7,
                                   by = "2 weeks"),
                 date_labels = "%d %b") +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Week ending",
         y = "Week admissions",
         title = forecast_date,
         col = "Model",
         fill = "Model") +
    theme_bw() +
    theme(legend.position = "top")
  
  # Add forecasts
  forecast_files <- list.files(path = here::here("data", "forecasts-raw"), recursive = TRUE)
  plot_files <- forecast_files[grepl(forecast_date, forecast_files)]
  
  dat_forecast <- purrr::map_df(.x = plot_files,
                                .f = ~ {
                                  
                                  out <- read_csv(file = here::here("data", "forecasts-raw", .x))
                                  
                                }) %>%
    bind_rows() %>%
    filter(quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95)) %>%
    mutate(quantile = paste0("q", quantile)) %>%
    rename(location = id) %>%
    pivot_wider(id_cols = -c(quantile, value), names_from = quantile)
  out <- g +
    geom_ribbon(data = dat_forecast,
                aes(x = date_horizon, y = q0.5,
                    ymin = q0.05, ymax = q0.95, fill = model),
                alpha = 0.4) +
    geom_point(data = dat_forecast,
               aes(x = date_horizon, y = q0.5, col = model)) +
    geom_line(data = dat_forecast,
              aes(x = date_horizon, y = q0.5, col = model)) 
  
  return(out)
  
}
