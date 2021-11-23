
# Get vector of country IDs that we will make admissions forecasts for
get_forecast_ids <- function(dat, forecast_date, max_trunc = 7) {
  
  # Get last reported week
  last_rep <- raw_dat %>%
    filter(!is.na(adm),
           week <= forecast_date) %>%
    group_by(location) %>%
    filter(week == max(week)) %>%
    ungroup() %>%
    filter(week >= forecast_date - 8*7) %>%
    select(location, last_rep = week) %>%
    mutate(trunc = as.numeric(forecast_date - last_rep)) %>%
    filter(trunc <= max_trunc)
  
  # Remove locations missing data in the last 8 weeks
  out <- raw_dat %>%
    left_join(last_rep, by = "location") %>%
    filter(week >= last_rep - 8*7,
           week < last_rep) %>%
    group_by(location, last_rep, trunc) %>%
    summarise(all_adm = sum(adm),
              .groups = "drop") %>% 
    filter(!is.na(all_adm)) %>%
    select(id = location, last_rep, trunc)
  
  return(out)
  
}

# Get samples from hub-ensemble quantile forecast
ensemble_samples <- function(dat, n.samples = 1000){
  
  id_int <- unique(dat$location)
  target_int <- unique(dat$target_end_date)
  
  message(paste0("Getting samples for ", id_int, " (", target_int, ")"))
  
  metalog_dn <- metalog(x = dat$value,
                        probs = dat$quantile,
                        bounds = c(0),
                        term_limit = 6)
  
  samples <- round(rmetalog(metalog_dn, n = n.samples, term = 5))
  
  out <- tibble(forecast_date = unique(dat$forecast_date),
                target = unique(dat$target),
                target_end_date = target_int,
                location = id_int,
                sample = 1:n.samples,
                value = samples)
  
  return(out)
  
}
