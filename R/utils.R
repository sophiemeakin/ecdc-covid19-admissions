
# Get vector of country IDs that we will make admissions forecasts for
get_forecast_ids <- function(dat, forecast_date) {
  
  out <- dat %>%
    filter(week >= forecast_date - 9*7,
           week < forecast_date) %>%
    group_by(location) %>%
    summarise(all_adm = sum(adm)) %>% 
    filter(!is.na(all_adm)) %>%
    pull(location)
  
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
