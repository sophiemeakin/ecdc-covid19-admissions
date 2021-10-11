
# Load case data ----------------------------------------------------------

load_jhu_cases <- function(weekly = TRUE){
  
  out <- read_csv(file = "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/JHU/truth_JHU-Incident%20Cases.csv") %>%
    rename(cases = value)
  
  if(weekly){
    
    out <- out %>%
      mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7),
             week = as.Date(week)) %>%
      group_by(location, location_name, week) %>%
      dplyr::summarise(n = n(),
                       cases = sum(cases, na.rm = TRUE),
                       .groups = "drop") %>%
      filter(n == 7) %>%
      select(-n)
    
    
  }
  
  return(out)
  
}


# Load admissions data ----------------------------------------------------

load_ecdc_hosps <- function(weekly = TRUE){
  
  out <- read_csv(file = "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/data-truth/ECDC/truth_ECDC-Incident%20Hospitalizations.csv") %>%
    rename(adm = value)
  
  if(weekly){
    
    out <- out %>%
      mutate(week = lubridate::floor_date(date, unit = "week", week_start = 7),
             week = as.Date(week)) %>%
      group_by(location, location_name, week) %>%
      dplyr::summarise(n = n(),
                       adm = sum(adm, na.rm = TRUE),
                       .groups = "drop") %>%
      filter(n == 7) %>%
      select(-n)
    
  }
  
  return(out)
  
}


# Load raw data -----------------------------------------------------------

load_data <- function(weekly = TRUE){
  
  case_data <- load_jhu_cases(weekly = weekly)
  adm_data <- load_ecdc_hosps(weekly = weekly)
  
  if(weekly){
    
    out <- case_data %>%
      left_join(adm_data, by = c("location", "location_name", "week"))
    
  } else {
    
    out <- case_data %>%
      left_join(adm_data, by = c("location", "location_name", "date"))
    
  }
  
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
