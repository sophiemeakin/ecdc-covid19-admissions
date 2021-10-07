ensemble_samples <- function(dat, n.samples = 1000){
  
  id_int <- unique(dat$location)
  target_int <- unique(dat$target_end_date)
  
  message(paste0("Getting samples for ", id_int, " (", target_int, ")"))
  
  metalog_dn <- metalog(x = dat$value,
                        probs = dat$quantile,
                        bounds = c(0),
                        term_limit = 6)
  
  samples <- round(rmetalog(metalog_dn, n = n.samples, term = 5))
  
  out <- tibble(forecast_date = unique(quantiles_in$forecast_date),
                target = unique(quantiles_in$target),
                target_end_date = .y,
                location = .x,
                sample = 1:n.samples,
                value = samples)
  
  return(out)
  
}
