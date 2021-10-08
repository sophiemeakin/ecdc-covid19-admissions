
# Set up ------------------------------------------------------------------

library(tidyverse)
library(rmetalog)

source(here::here("R", "load_data_fns.R"))
source(here::here("R", "forecast_fns.R"))

# Forecast date (a Sunday) - the last day of observed data
fdate <- as.Date("2021-09-25")


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
         date = forecast_date + 1 + 7*(horizon - 1)) %>%
  select(id = location, date, cases = value)


grid <- expand_grid(id = unique(raw_case_forecast$raw_forecast$location),
                    target_date = unique(raw_case_forecast$raw_forecast$target_end_date))
forecast_samples <- map2_df(.x = grid$id,
                            .y = grid$target_date,
                            .f = ~ {
                              
                              dat_in <- raw_case_forecast$raw_forecast %>%
                                filter(location == .x,
                                       target_end_date == .y)
                              
                              out <- ensemble_samples(dat = dat_in, )
                              
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
  select(quantile_label)

file_name <- paste0("timeseries_ensemble_", fdate, ".csv")
write_csv(tsensemble_summary,
          file = here::here("forecasts", "timeseries_ensemble", file_name))


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
  mutate(date_horizon = forecast_from + (7*horizon))




# Case-convolution --------------------------------------------------------





# Vis ---------------------------------------------------------------------

plot_summary <- tsensemble_summary %>%
  bind_rows(arimareg_summary) %>%
  filter(quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95),
         quantile_label != "upper_0") %>%
  select(-quantile) %>%
  pivot_wider(id_cols = -c(quantile_label, value), names_from = quantile_label)

## Observed data
g <- dat_in %>%
  filter(date >= fdate - 8*7) %>%
  ggplot(aes(x = date + 7, y = adm)) +
  geom_line() +
  facet_wrap(. ~ id, scales = "free_y") +
  scale_x_date(breaks = seq.Date(from = fdate - 8*7,
                                 to = fdate + 4*7,
                                 by = "2 weeks"),
               date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Week ending",
       y = "Week admissions",
       col = "Model",
       fill = "Model") +
  theme_bw() +
  theme(legend.position = "top")
  
## Add forecast data
g +
  geom_ribbon(data = plot_summary,
              aes(x = date_horizon, y = lower_0,
                  ymin = lower_90, ymax = upper_90, fill = model),
              alpha = 0.4) + 
  geom_point(data = plot_summary,
             aes(x = date_horizon, y = lower_0, col = model)) +
  geom_line(data = plot_summary,
            aes(x = date_horizon, y = lower_0, col = model)) 

