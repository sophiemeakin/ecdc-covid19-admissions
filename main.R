
# Date --------------------------------------------------------------------

# a Saturday - the last day of observed data
fdate <- lubridate::floor_date(lubridate::today(),
                               unit = "week",
                               week_start = 6)


# Set up ------------------------------------------------------------------

library(tidyverse)
library(rmetalog)
library(EpiNow2)

# Using functions from covid19-hospital-activity (Trust-level admissions forecasts)
devtools::source_url("https://raw.githubusercontent.com/epiforecasts/covid19-hospital-activity/main/R/forecast_fns.R")
devtools::source_url("https://raw.githubusercontent.com/seabbs/regional-secondary/master/regional-secondary.R")

devtools::load_all()

if (!dir.exists(here::here("data", "figures"))) {
  dir.create(here::here("data", "figures"))
}

unlink(here::here("data-processed", ""), recursive = TRUE)
dir.create(here::here("data-processed"))
dir.create(here::here("data-processed", "epiforecasts-caseconv"))
dir.create(here::here("data-processed", "epiforecasts-arimareg"))
dir.create(here::here("data-processed", "epiforecasts-tsensemble"))

# Load data ---------------------------------------------------------------

raw_dat <- load_data(end_date = fdate)

fcast_ids <- get_forecast_ids(dat = raw_dat,
                              forecast_date = fdate,
                              max_trunc = 30)

raw_case_forecast <- load_hub_ensemble(forecast_date = (fdate + 2),
                                       locs = fcast_ids$id)


# Reshape data ------------------------------------------------------------

# Dates of forecast horizons (to allow for truncated data)
fhorizons <- seq.Date(from = fdate - 2 * 7, to = fdate + 4 * 7, by = "week")

# Observed data
obs_dat <- raw_dat %>%
  filter(location %in% fcast_ids$id) %>%
  select(id = location, date = week, cases, adm)

# Forecast median (point)
forecast_point <- raw_case_forecast$point_forecast %>%
  select(id = location, date = target_end_date, cases = value)

# Forecast samples
grid <- expand_grid(id = unique(raw_case_forecast$raw_forecast$location),
                    target_date = unique(raw_case_forecast$raw_forecast$target_end_date))
forecast_samples <- map2_df(.x = grid$id,
                            .y = grid$target_date,
                            .f = ~ {
                              
                              dat_in <- raw_case_forecast$raw_forecast %>%
                                filter(location == .x,
                                       target_end_date == .y)
                              
                              out <- get_ensemble_samples(dat = dat_in)
                              
                            }) %>%
  bind_rows() %>%
  select(id = location, date = target_end_date, sample, cases = value)

# Data for ARIMA models 
dat <- obs_dat %>%
  bind_rows(forecast_point) %>%
  group_by(id) %>%
  mutate(date = as.Date(date),
         cases_lag1 = lag(cases, 1))


# Vis hub-ensemble case forecast ------------------------------------------

g_case <- plot_ensemble(dat_obs = raw_dat,
                        dat_for = raw_case_forecast$raw_forecast,
                        regions = fcast_ids$id,
                        forecast_date = fdate)

ggsave(plot = g_case,
       filename = here::here("data", "figures", "current_case_forecast.pdf"),
       height = 9, width = 14, units = "in", dpi = 500)


# Time series ensemble ----------------------------------------------------

tsensemble_samples <- purrr::map_df(.x = sort(unique(fcast_ids$trunc)),
              .f = ~ {
                
                dat_int <- dat %>%
                  filter(id %in% fcast_ids$id[which(fcast_ids$trunc == .x)])
                
                fdate_int <- unique(fcast_ids$last_rep[which(fcast_ids$trunc == .x)])
                
                out_samples <- timeseries_samples(data = dat_int,
                                                  yvar = "adm",
                                                  horizon = 4 + .x/7,
                                                  train_from = fdate_int - 8*7,
                                                  forecast_from = fdate_int,
                                                  models = "aez")
                
                return(out_samples)
                
              }) %>%
  bind_rows() %>%
  ungroup() %>%
  mutate(model = "Time series ensemble")

tsensemble_summary <- forecast_summary(samples = tsensemble_samples,
                                       quantiles = c(0.01, 0.025,
                                                     seq(from = 0.05, to = 0.95, by = 0.05),
                                                     0.975, 0.99)) %>%
  ungroup() %>%
  mutate(date_horizon = forecast_from + (7*horizon),
         horizon = as.numeric(date_horizon - fdate)/7,
         forecast_from = fdate) %>%
  filter(date_horizon %in% fhorizons,
         quantile_label != "upper_0") %>%
  select(-quantile_label)

file_name <- paste0("timeseries_ensemble_", fdate, ".csv")
write_csv(tsensemble_summary,
          file = here::here("data", "forecasts-raw", "timeseries_ensemble", file_name))

format_forecast(forecast_summary = tsensemble_summary,
                file_name = paste0(fdate + 1, "-epiforecasts-tsensemble.csv"),
                file_path = here::here("data-processed", "epiforecasts-tsensemble"))


# ARIMA regression --------------------------------------------------------

arimareg_samples <- purrr::map_df(.x = sort(unique(fcast_ids$trunc)),
              .f = ~ {
                
                dat_int <- dat %>%
                  filter(id %in% fcast_ids$id[which(fcast_ids$trunc == .x)])
                
                fdate_int <- unique(fcast_ids$last_rep[which(fcast_ids$trunc == .x)])
                
                out_samples <- timeseries_samples(data = dat_int,
                                                  yvar = "adm",
                                                  xvars = c("cases_lag1"),
                                                  horizon = 28 + .x,
                                                  train_from = fdate_int - 8*7,
                                                  forecast_from = fdate_int,
                                                  models = "a")
                
                return(out_samples)
                
              }) %>%
  bind_rows() %>%
  mutate(model = "ARIMA regression")
  
arimareg_summary <- forecast_summary(samples = arimareg_samples,
                                       quantiles = c(0.01, 0.025,
                                                     seq(from = 0.05, to = 0.95, by = 0.05),
                                                     0.975, 0.99)) %>%
  mutate(date_horizon = forecast_from + (7*horizon),
         horizon = as.numeric(date_horizon - fdate)/7,
         forecast_from = fdate,) %>%
  filter(date_horizon %in% fhorizons,
         quantile_label != "upper_0") %>%
  select(-quantile_label)

file_name <- paste0("arimareg_", fdate, ".csv")
write_csv(arimareg_summary,
          file = here::here("data", "forecasts-raw", "arima_regression", file_name))

format_forecast(forecast_summary = arimareg_summary,
                file_name = paste0(fdate + 1, "-epiforecasts-arimareg.csv"),
                file_path = here::here("data-processed", "epiforecasts-arimareg"))


# Case-convolution --------------------------------------------------------

convolution_samples <- purrr::map_df(.x = sort(unique(fcast_ids$trunc)),
              .f = ~ {
                
                fdate_int <- unique(fcast_ids$last_rep[which(fcast_ids$trunc == .x)])
                dat_int <- format_conv_data(obs_dat = dat %>% filter(id %in% fcast_ids$id[which(fcast_ids$trunc == .x)]),
                                            forecast_dat = forecast_samples %>% filter(id %in% fcast_ids$id[which(fcast_ids$trunc == .x)]),
                                            cut_date = fdate_int,
                                            fdate = fdate)
                
                convolution_forecast <- regional_secondary(reports = data.table::data.table(dat_int$cc_obs),
                                                           case_forecast = data.table::data.table(dat_int$cc_forecast),
                                                           secondary = secondary_opts(type = "incidence"),
                                                           delays = delay_opts(list(
                                                             mean = 1, mean_sd = 0.5,
                                                             sd = 0.5, sd_sd = 0.25, max = 4
                                                           )),
                                                           obs = EpiNow2::obs_opts(week_effect = FALSE,
                                                                                   scale = list(mean = 0.2, sd = 0.1)),
                                                           burn_in = 2,
                                                           control = list(adapt_delta = 0.99, max_treedepth = 15),
                                                           return_fit = FALSE,
                                                           return_plots = FALSE,
                                                           verbose = TRUE)
                
                out_samples <- convolution_forecast$samples %>%
                  dplyr::filter(date > fdate_int) %>%
                  dplyr::mutate(forecast_from = fdate_int,
                                horizon = as.integer(date - forecast_from),
                                model = "Case-convolution") %>%
                  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
                
                return(out_samples)
                
              }) %>%
  bind_rows()

convolution_summary <- forecast_summary(samples = convolution_samples,
                                              quantiles = c(0.01, 0.025,
                                                            seq(from = 0.05, to = 0.95, by = 0.05),
                                                            0.975, 0.99)) %>%
  mutate(horizon = as.numeric(date_horizon - fdate)/7,
         forecast_from = fdate) %>%
  filter(date_horizon %in% fhorizons,
         quantile_label != "upper_0") %>%
  select(-quantile_label)


file_name <- paste0("convolution_", fdate, ".csv")
write_csv(convolution_summary,
          file = here::here("data", "forecasts-raw", "case_convolution", file_name))

format_forecast(forecast_summary = convolution_summary,
                file_name = paste0(fdate + 1, "-epiforecasts-caseconv.csv"),
                file_path = here::here("data-processed", "epiforecasts-caseconv"))


# Vis model forecasts -----------------------------------------------------

g_admissions <- plot_forecasts(dat_obs = raw_dat,
                               forecast_date = fdate,
                               regions = fcast_ids$id,
                               models = c("Time series ensemble",
                                          "ARIMA regression",
                                          "Case-convolution"))

ggsave(plot = g_admissions,
       filename = here::here("data", "figures", "current_admissions_forecast.pdf"),
       height = 9, width = 14, units = "in", dpi = 500)

file.copy(
  Sys.glob(here::here("data-processed", "*", "*")),
  here::here("data", "forecasts-format")
)

