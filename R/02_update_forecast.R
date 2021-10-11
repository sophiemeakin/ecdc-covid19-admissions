
# Date --------------------------------------------------------------------

# a Saturday - the last day of observed data
fdate <- as.Date("2021-10-09")


# Set up ------------------------------------------------------------------

library(tidyverse)
library(rmetalog)

source(here::here("R", "load_data_fns.R"))
source(here::here("R", "utils.R"))

# Using functions from covid19-hospital-activity (Trust-level admissions forecasts)
devtools::source_url("https://raw.githubusercontent.com/epiforecasts/covid19-hospital-activity/main/R/forecast_fns.R")
devtools::source_url("https://raw.githubusercontent.com/seabbs/regional-secondary/master/regional-secondary.R")


# Load data ---------------------------------------------------------------

raw_dat <- load_data()

fcast_ids <- raw_dat %>%
  filter(week >= fdate - 12*7,
         week < fdate) %>%
  group_by(location) %>%
  summarise(all_adm = sum(adm)) %>%
  filter(!is.na(all_adm)) %>%
  pull(location)

raw_case_forecast <- load_hub_ensemble(forecast_date = (fdate + 2),
                                       locs = fcast_ids)


# Reshape data ------------------------------------------------------------

forecast_point <- raw_case_forecast$point_forecast %>%
  mutate(forecast_date = fdate,
         horizon = as.numeric(substr(target, 1, 1)),
         date = target_end_date - 6) %>%
  select(id = location, date, cases = value)


grid <- expand_grid(id = unique(raw_case_forecast$raw_forecast$location),
                    target_date = unique(raw_case_forecast$raw_forecast$target_end_date))
forecast_samples <- map2_df(.x = grid$id,
                            .y = grid$target_date,
                            .f = ~ {
                              
                              dat_in <- raw_case_forecast$raw_forecast %>%
                                filter(location == .x,
                                       target_end_date == .y)
                              
                              out <- ensemble_samples(dat = dat_in)
                              
                            }) %>%
  bind_rows()

dat <- raw_dat %>%
  filter(week < fdate) %>%
  select(id = location, date = week, cases, adm) %>%
  bind_rows(forecast_point)

# Time series ensemble ----------------------------------------------------

dat_in <- dat %>%
  filter(id %in% fcast_ids) %>%
  group_by(id) %>%
  mutate(date = as.Date(date),
         cases_lag1 = lag(cases, 1),
         cases_lag2 = lag(cases, 2))

tsensemble_samples <- timeseries_samples(data = dat_in,
                                         yvar = "adm",
                                         horizon = 4,
                                         train_from = fdate - 8*7,
                                         forecast_from = fdate,
                                         models = "aez") %>%
  mutate(model = "Time series ensemble")
tsensemble_summary <- forecast_summary(samples = tsensemble_samples,
                                       quantiles = c(0.01, 0.025,
                                                     seq(from = 0.05, to = 0.95, by = 0.05),
                                                     0.975, 0.99)) %>%
  mutate(date_horizon = forecast_from + (7*horizon)) %>%
  filter(quantile_label != "upper_0") %>%
  select(-quantile_label)

file_name <- paste0("timeseries_ensemble_", fdate, ".csv")
write_csv(tsensemble_summary,
          file = here::here("data", "forecasts", "timeseries_ensemble", file_name))


# ARIMA regression --------------------------------------------------------

arimareg_samples <- timeseries_samples(data = dat_in,
                                       yvar = "adm",
                                       xvars = c("cases_lag1"),
                                       horizon = 28,
                                       train_from = fdate - 8*7,
                                       forecast_from = fdate,
                                       models = "a") %>%
  mutate(model = "ARIMA regression")
arimareg_summary <- forecast_summary(samples = arimareg_samples,
                                       quantiles = c(0.01, 0.025,
                                                     seq(from = 0.05, to = 0.95, by = 0.05),
                                                     0.975, 0.99)) %>%
  mutate(date_horizon = forecast_from + (7*horizon)) %>%
  filter(quantile_label != "upper_0") %>%
  select(-quantile_label)

file_name <- paste0("arimareg_", fdate, ".csv")
write_csv(arimareg_summary,
          file = here::here("data", "forecasts", "arima_regression", file_name))


# Case-convolution --------------------------------------------------------

dat_obs <- dat_in %>%
  select(region = id, date, primary = cases, secondary = adm) %>%
  filter(date >= fdate - 12*7,
         date < fdate)

dat_for <- forecast_samples %>%
  select(region = location, date = target_end_date, sample, cases = value)

convolution_forecast <- regional_secondary(reports = data.table::data.table(dat_obs),
                                           case_forecast = data.table::data.table(dat_for),
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
# Model samples and summary
convolution_samples <- convolution_forecast$samples %>%
  dplyr::filter(date > fdate) %>%
  dplyr::mutate(forecast_from = fdate,
                horizon = as.integer(date - forecast_from),
                model = "Case-convolution") %>%
  dplyr::select(id = region, sample, horizon, value, forecast_from, model)
convolution_summary <- forecast_summary(samples = convolution_samples,
                                              quantiles = c(0.01, 0.025,
                                                            seq(from = 0.05, to = 0.95, by = 0.05),
                                                            0.975, 0.99)) %>%
  filter(quantile_label != "upper_0") %>%
  select(-quantile_label)


file_name <- paste0("convolution_", fdate, ".csv")
write_csv(convolution_summary,
          file = here::here("data", "forecasts", "case_convolution", file_name))
