
# Load case data ----------------------------------------------------------

load_jhu_cases <- function(weekly = TRUE, end_date){
  
  dat_raw <- read_csv(file = "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") %>%
    rename(cases = value)
  
  min_max_date <- dat_raw %>%
    group_by(location) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    filter(date == min(date)) %>%
    pull(date) %>%
    unique()
  
  if(as.Date(end_date) < max(dat_raw$date)) {
    end_date <- max(dat_raw$date)
  }
  
  out <- dat_raw %>%
    filter(date <= end_date) %>%
    complete(nesting(location, location_name),
             date = seq.Date(from = min_max_date, to = as.Date(end_date), by = "day")) %>%
    arrange(location, date) %>%
    group_by(location) %>%
    mutate(cases_lag = lag(cases, 7),
           cases = ifelse(is.na(cases), cases_lag, cases)) %>%
    ungroup() %>%
    select(-cases_lag)
  
  if(weekly){
    
    out <- out %>%
      mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7),
             week = as.Date(week) + 6) %>%
      group_by(location, location_name, week) %>%
      dplyr::summarise(n = n(),
                       cases = sum(cases),
                       .groups = "drop") %>%
      filter(n == 7) %>%
      select(-n)
    
    
  }
  
  return(out)
  
}


# Load admissions data ----------------------------------------------------

load_ecdc_hosps <- function(){
  
  dat_raw <- read_csv(file = "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/ECDC/truth_ECDC-Incident%20Hospitalizations.csv") %>%
    rename(adm = value)
  
  out <- dat_raw %>%
    rename(week = date) %>%
    select(location_name, location, week, adm)
  
  return(out)
  
}


# Load raw data -----------------------------------------------------------

load_data <- function(end_date){
  
  case_data <- load_jhu_cases(weekly = TRUE, end_date = end_date)
  adm_data <- load_ecdc_hosps()
  
  out <- case_data %>%
    left_join(adm_data, by = c("location", "location_name", "week"))
  
  return(out)
  
}


# Load ensemble case forecast ---------------------------------------------

load_hub_ensemble <- function(forecast_date = "2021-09-27", locs){
  
  url <- paste0(
    "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-processed/EuroCOVIDhub-ensemble/",
    forecast_date,
    "-EuroCOVIDhub-ensemble.csv"
  )
  
  dat <- read_csv(file = url)
  
  if(missing(locs)){
    locs <- unique(dat$location)
  }
  
  # Raw forecast
  out <- dat %>%
    filter(grepl("inc case", target),
           type == "quantile",
           location %in% locs)
  
  # Point forecast
  out_point <- out %>%
    filter(quantile == 0.5)
  
  return(list(raw_forecast = out,
              point_forecast = out_point))
  
}
