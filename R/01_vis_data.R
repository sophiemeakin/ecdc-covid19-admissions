
# Set up ------------------------------------------------------------------

library(tidyverse)

source(here::here("R", "load_data_fns.R"))

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

# Plot (scaled) observed data ---------------------------------------------

g_observed <- raw_dat %>%
  filter(location %in% fcast_ids,
         week >= as.Date("2020-12-01"),
         week < fdate) %>%
  group_by(location, location_name) %>%
  mutate(cases_std = cases/sd(cases, na.rm = TRUE),
         adm_std = adm/sd(adm, na.rm = TRUE)) %>%
  select(contains("location"), week, contains("std")) %>%
  pivot_longer(cols = contains("std")) %>%
  ggplot(aes(x = week, y = value, col = name)) +
  geom_line() +
  facet_wrap(. ~ location_name) +
  theme_bw()

ggsave(plot = g_observed,
       filename = here::here("data", "figures", "observed_case_adm.pdf"),
       height = 9, width = 14, units = "in", dpi = 500)


# Plot case forecast ------------------------------------------------------

## Observed data
g_case <- raw_dat %>%
  filter(location %in% fcast_ids,
         week >= fdate - 8*7,
         week <= fdate) %>%
  ggplot(aes(x = week, y = cases)) +
  geom_line() +
  facet_wrap(. ~ location, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Week ending",
       y = "Week cases",
       col = "Model",
       fill = "Model") +
  theme_bw() +
  theme(legend.position = "top")

plot_case_forecast <- raw_case_forecast$raw_forecast %>%
  filter(quantile %in% c(0.05, 0.25, 0.5, 0.75, 0.95)) %>%
  mutate(quantile = paste0("q", quantile)) %>%
  pivot_wider(id_cols = -c(quantile, value), names_from = quantile)

g_case <- g_case + 
  geom_ribbon(data = plot_case_forecast,
            aes(x = target_end_date - 6, y = q0.5, ymin = q0.05, ymax = q0.95),
            fill = "grey50", alpha = 0.4) +
  geom_ribbon(data = plot_case_forecast,
              aes(x = target_end_date - 6, y = q0.5, ymin = q0.25, ymax = q0.75),
              fill = "grey50", alpha = 0.4) +
  geom_point(data = plot_case_forecast,
             aes(x = target_end_date - 6, y = q0.5)) + 
  geom_line(data = plot_case_forecast,
            aes(x = target_end_date - 6, y = q0.5))


ggsave(plot = g_case,
       filename = here::here("data", "figures", "forecast_case_ensemble.pdf"),
       height = 9, width = 14, units = "in", dpi = 500)

