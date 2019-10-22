library(eurostat)
library(tidyverse)
library(xlsx)


USE_CACHE = TRUE


getEurostatData <- function() {
  df <-
    get_eurostat('hlth_rs_prsrg',
                 stringsAsFactors = FALSE,
                 cache = USE_CACHE)
  
  df$year <- lubridate::year(df$time)
  df$country.code <- str_sub(df$geo, 1, 2)
  
  df <-
    df %>% filter(isco08 %in% c('OC222_322', 'OC221')) %>% filter(unit ==
                                                                    'P_HTHAB' &
                                                                    year > 2004)
  
  df.region <- df %>% filter(nchar(geo) >= 3)
  
  labels_OC  <- label_eurostat(df.region %>% select(isco08))
  colnames(labels_OC) <- c('variable')
  
  regions <-
    label_eurostat(df.region %>% select(geo), fix_duplicated = T)
  colnames(regions) <- c('region')
  countries <-
    label_eurostat(df.region$country.code,
                   dic = 'geo',
                   fix_duplicated = T) %>% as.data.frame()
  colnames(countries) <- c('country')
  
  df.clean <-
    cbind(labels_OC, df.region, regions, countries) %>% select(-unit,-time)
  df.clean$country <- as.character(df.clean$country)
  df.clean$country[grepl("Germany", df.clean$country, ignore.case = FALSE)] <-
    "Germany"
  
  
  
  
  df.physicians <-
    df.clean %>% filter(isco08 == 'OC221') #%>% spread(year,values)
  df.nurses <-
    df.clean %>% filter(isco08 == 'OC222_322')#%>% spread(year,values)
  
  countries <- read.csv('data/countries_filter.csv', sep = ';')
  
  
  
  ## filter on specific countries
  
  df.physicians <-
    df.physicians %>% filter(country %in% unique(countries$OC221))
  df.nurses <-
    df.nurses %>% filter(country %in% unique(countries$OC222_322))
  
  data <- NULL
  data$df.physicians <- df.physicians
  data$df.nurses <- df.nurses
  
  data$df <- df
  data$countries <- countries
  
  # first get the health data
  df.countries <- data$df %>% filter(str_length(geo) == 2)
  countries.phys = label_eurostat(df.countries %>% select(geo),
                                  dic = 'geo',
                                  fix_duplicated = T)
  colnames(countries.phys) <- c('country.name')
  df.countries <- cbind(countries.phys, df.countries)
  df.countries$country.name[grepl("Germany", df.countries$country.name, ignore.case =
                                    FALSE)] <- "Germany"
  
  countries.phys <-
    df.countries %>% filter(country.name %in% data$countries$OC221) %>% filter(isco08 ==
                                                                                 'OC221') %>% select(-time, -country.code, -unit)
  country.phys.label <-
    label_eurostat(countries.phys %>% select(isco08))
  names(country.phys.label) <- c('variable')
  countries.phys <- cbind(country.phys.label, countries.phys)
 
  
  
  countries.nurses <-
    df.countries %>% filter(country.name %in% data$countries$OC222_322) %>% filter(isco08 ==
                                                                                     'OC222_322') %>% select(-time, -country.code, -unit)
  country.nurses.label <-
    label_eurostat(countries.nurses %>% select(isco08))
  names(country.nurses.label) <- c('variable')
  countries.nurses <- cbind(country.nurses.label, countries.nurses)
  
  
  data$countries.phys <- countries.phys
  data$countries.nurses <- countries.nurses
  
  return (data)
  
  
  
}