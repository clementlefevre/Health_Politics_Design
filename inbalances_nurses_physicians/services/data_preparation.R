


library(eurostat)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)


# load data from eurostats and prepare a dataframe for visualization and modeling


data("eu_countries")
countries <- read.csv('data/countries_list.csv', header = F)


load.countries.info <- function() {
  colnames(countries) <- c('name')
  countries.with.code <- merge(countries, eu_countries, by = 'name')
  
  norway <- data.frame('Norway', 'NO', '')
  names(norway) <- names(countries.with.code)
  turkey <- data.frame('Turkey', 'TR', '')
  names(turkey) <- names(countries.with.code)
  
  switzerland <- data.frame('Switzerland', 'CH', '')
  names(switzerland) <- names(countries.with.code)
  
  
  countries.with.code <- rbind(countries.with.code, norway, turkey, switzerland)
  codes.countries <- countries.with.code$code
  
  return (countries.with.code)
  
  
}


load.pop <- function(geo.level=4){
  
  if(geo.level==2){
    table <-  'tps00001'
  } else if(geo.level==4){
    table<-'tgs00096'
  }
  
  
  df <- get_eurostat(table,  stringsAsFactors = FALSE)
  
  return(df)
  
}



load.pop.density <- function(geo.level=4) {
  
  if(geo.level==2){
    table <- "tps00003"
  } else if(geo.level==4){
    table<-'tgs00024'
  }
  df <- get_eurostat(table,  stringsAsFactors = FALSE)
 
  df <- df %>% select(-unit)
  colnames(df) <- c('geo', 'time', 'HAB_KM2')
  
  df$year <- lubridate::year(df$time)
  df <- df %>% select(-time)
  return(df)
  
  
}
load.pop.structure<- function(geo.level){
  
  if(geo.level==2){
    table <-  'demo_pjangroup'
  } else if(geo.level==4){
    table<-'demo_r_pjangroup'
  }
 
  
  df <- get_eurostat(table,  stringsAsFactors = FALSE)
  
  df.name <- label_eurostat(df %>% dplyr::select(age))
  df.name <- cbind(df$age,df.name)
  colnames(df.name)<- c('age','age.label')
  df.name <- df.name %>% distinct(age,age.label)
  
  df <- df %>% filter(nchar(geo) == geo.level)
  df$year <- lubridate::year(df$time)
  
  df<- df %>% filter(year>=2005 & year <=2016)
  
  exclude.age <- NULL # c('TOTAL','UNK','Y_GE75','Y_GE80')
  
  ages.of.interest <- c('TOTAL', 'UNK', 'Y10-14', 'Y15-19', 'Y20-24', 'Y25-29', 'Y30-34', 'Y35-39', 'Y40-44', 'Y45-49', 'Y5-9', 'Y50-54', 'Y55-59', 'Y60-64', 'Y65-69', 'Y70-74', 'Y75-79', 'Y80-84', 'Y_GE75', 'Y_GE80', 'Y_GE85', 'Y_LT5')
  df <- df %>% filter(age  %in% ages.of.interest & sex=='T') %>% group_by(geo,sex,year) %>% mutate(age.ratio=values/sum(values)*100)
  df <- df %>% select(-unit,-time,-values)
  
  df <- df %>% mutate(sex.age = paste(sex,age,sep='_')) %>% ungroup() %>%   select(-age,-sex)
  df <-df %>% spread(sex.age,age.ratio, fill=0)
  return (df)
  
}

load.gdp <- function(geo.level=4){
  
  if(geo.level==2){
    table <-  'nama_10_pc'
    df <- get_eurostat(table,  stringsAsFactors = FALSE)
    df.gdp <- df %>% filter(na_item=='B1GQ' & unit=='CP_EUR_HAB')
    df.gdp.labels <- label_eurostat(df.gdp %>% select(unit,na_item))
    colnames(df.gdp.labels)<- paste0('label.',colnames(df.gdp.labels))
    df.gdp <- cbind(df.gdp,df.gdp.labels)
    df.gdp <- df.gdp
    
    
  } else if(geo.level==4){
    table<-'nama_10r_2gdp' 
    df <- get_eurostat(table,  stringsAsFactors = FALSE)
    df.gdp <- df %>% filter(unit=='EUR_HAB') %>% filter(nchar(geo) == geo.level)
    df.gdp.labels <- label_eurostat(df.gdp %>% select(unit))
    colnames(df.gdp.labels)<- paste0('label.',colnames(df.gdp.labels))
    df.gdp <- cbind(df.gdp,df.gdp.labels)
    
    
  }
  
  df.gdp <- df.gdp %>% select(geo,time,values)
  df.gdp$year <- year(df.gdp$time) 
  df.gdp <- rename(df.gdp,EUR_HAB=values) %>% select(geo,year,EUR_HAB,-time)
  
  return(df.gdp)
  
  
 return (NULL)
  
}



load.nurses.phys.data <-
  function(geo.level = 4,cache=TRUE) {
    data <- NULL
    data$codes.countries <- load.countries.info()
    
    df <- get_eurostat('hlth_rs_prsrg',  stringsAsFactors = FALSE,cache=cache)
    
    df <-
      df %>% filter(isco08 %in%        c('OC222_322', 'OC221')) %>% filter(nchar(geo) == geo.level) %>% filter(unit ==
                                                                                                                 'P_HTHAB')
    
    df <-
      df %>% filter(grepl(paste(data$codes.countries$code, collapse = "|"), geo))
    
    df$country.code <- str_sub(df$geo, 1, 2)
    
    df <- df %>% filter(year(time) > 2004)
    
    nuts2.names <- label_eurostat(df %>% select(geo, unit))
    
    colnames(nuts2.names) <- c('geo.name', 'unit.name')
    df <- cbind(df, nuts2.names)
    
    df <-
      merge(df, data$codes.countries, by.x = 'country.code', by.y = 'code')
    df$year <- lubridate::year(df$time)
    df<- df %>% select(-time)
    df <- df %>% arrange(year)
    
    df <- df%>% spread(isco08,values)
    df$ratio.nurses.doct <- df$OC222_322/df$OC221
    
    df.density <- load.pop.density(geo.level)
    df <- merge(df,df.density,by=c('geo','year'))
    
    df.pop.structure <- load.pop.structure(geo.level = geo.level)
    df <- merge(df,df.pop.structure,by=c('geo','year'), suffixes=c('.density','.pop.struct'))
    
  
    df.gdp <- load.gdp(geo.level)
    df <- merge(df,df.gdp,by=c('geo','year'))
    colnames(df)<- make.names(colnames(df))
  
    
    data$df <- df
    
    return (data)
    
  }




compute.CAGR <- function(data) {
  groupy <-
    data %>% group_by(geo, isco08) %>% mutate(last.doctors.density = last(values))
  df.cagr <- groupy  %>%
    group_by(geo, isco08) %>%
    dplyr::summarise(
      growth.percent = (last(values) - first(values)) / first(values) * 100,
      cagr = ((last(values) / first(values)) ^ (1 / (
        max(year) - min(year)
      )) - 1) *
        100
    )
  
  df.cagr <-
    merge(df.cagr, distinct(data[c('geo', 'name')]), by = 'geo', all.x = FALSE)
  
  last.doctors.density <-
    data %>% filter(isco08 == 'OC221') %>% select(geo, year, isco08, values) %>% group_by(geo, isco08) %>% summarise(last.doct.density =
                                                                                                                       last(values)) %>% select(-isco08)
  
  df.cagr <- merge(df.cagr,
                   last.doctors.density,
                   by = 'geo',
                   all.x = T)
  return (df.cagr)
}
