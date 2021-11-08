
# Date --------------------------------------------------------------------

# a Saturday - the last day of observed data
fdate <- lubridate::today()
lubridate::wday(fdate) <- "Saturday"
fdate <- fdate - lubridate::weeks(1L)

# Set up ------------------------------------------------------------------

library(tidyverse)
library(rmetalog)
library(EpiNow2)

# Using functions from covid19-hospital-activity (Trust-level admissions forecasts)
devtools::source_url("https://raw.githubusercontent.com/epiforecasts/covid19-hospital-activity/main/R/forecast_fns.R")
devtools::source_url("https://raw.githubusercontent.com/seabbs/regional-secondary/master/regional-secondary.R")

source(here::here("R", "data_fns.R"))
source(here::here("R", "plot_fns.R"))
source(here::here("R", "format_fns.R"))
source(here::here("R", "utils.R"))

if (!dir.exists(here::here("data", "figures"))) {
  dir.create(here::here("data", "figures"))
}

unlink(here::here("data", "latest-forecasts"), recursive = TRUE)
dir.create(here::here("data", "latest-forecasts"))

# Load data ---------------------------------------------------------------

raw_dat <- load_data(weekly = TRUE, end_date = fdate)

fcast_ids <- get_forecast_ids(dat = raw_dat,
                              forecast_date = fdate)

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
  bind_rows(forecast_point) %>%
  filter(id %in% fcast_ids) %>%
  group_by(id) %>%
  mutate(date = as.Date(date),
         cases_lag1 = lag(cases, 1),
         cases_lag2 = lag(cases, 2))


# Vis hub-ensemble case forecast ------------------------------------------

g_case <- plot_ensemble(dat_obs = raw_dat,
                        dat_for = raw_case_forecast$raw_forecast,
                        regions = fcast_ids,
                        forecast_date = fdate)

ggsave(plot = g_case,
       filename = here::here("data", "figures", "current_case_forecast.pdf"),
       height = 9, width = 14, units = "in", dpi = 500)


# Time series ensemble ----------------------------------------------------

tsensemble_samples <- timeseries_samples(data = dat,
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
          file = here::here("data", "forecasts-raw", "timeseries_ensemble", file_name))

format_forecast(forecast_summary = tsensemble_summary,
                file_name = paste0(fdate + 1, "-epiforecasts-tsensemble.csv"),
                file_path = here::here("data", "latest-forecasts"))


# ARIMA regression --------------------------------------------------------

arimareg_samples <- timeseries_samples(data = dat,
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
          file = here::here("data", "forecasts-raw", "arima_regression", file_name))

format_forecast(forecast_summary = arimareg_summary,
                file_name = paste0(fdate + 1, "-epiforecasts-arimareg.csv"),
                file_path = here::here("data", "latest-forecasts"))


# Case-convolution --------------------------------------------------------

dat_obs <- dat %>%
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
  mutate(horizon = horizon/7) %>%
  filter(quantile_label != "upper_0") %>%
  select(-quantile_label)


file_name <- paste0("convolution_", fdate, ".csv")
write_csv(convolution_summary,
          file = here::here("data", "forecasts-raw", "case_convolution", file_name))

format_forecast(forecast_summary = convolution_summary,
                file_name = paste0(fdate + 1, "-epiforecasts-caseconv.csv"),
                file_path = here::here("data", "latest-forecasts"))


# Vis model forecasts -----------------------------------------------------

g_admissions <- plot_forecasts(dat_obs = raw_dat,
                               forecast_date = fdate,
                               regions = fcast_ids)

ggsave(plot = g_admissions,
       filename = here::here("data", "figures", "current_admissions_forecast.pdf"),
       height = 9, width = 14, units = "in", dpi = 500)

file.copy(
  Sys.glob(here::here("data", "latest-forecasts", "*")),
  here::here("data", "forecasts-format"),
  recursive = TRUE
)

