

prepare.data <- function(df.nuts){
  
  data.density <- df.nuts %>% drop_na() %>% arrange(year)
  data.density$legend.name <- str_replace(data.density$geo.name,"\\(NUTS 2013","")
  data.density$legend.name <- str_replace(data.density$legend.name,"\\)","")
  data.density <- data.density %>% group_by(geo) %>% mutate(text.plot = paste0(legend.name,' (',first(year),'-',last(year),')'))
  
  return (data.density)
}

plot.density.scatter.timeline.countries <- function(data) {
  
  
  data.range.start <- data %>%
    group_by(geo) %>%
    arrange(year) %>%
    filter(row_number() == 1)
  
  data.range.stop <- data %>%
    group_by(geo) %>%
    arrange(year) %>%
    filter(row_number() == n())
  
  data.range.stop.second <- data.range.stop
  
  over.countries <- as.factor(c('IT','ES','BG','CZ','SK','HU','HR','FI','SE','NO'))
  
  data.range.stop <-
    data.range.stop %>% mutate(text.plot = ifelse(country.code %in% over.countries, country.code, text.plot))
  
  data.range.stop.second <- data.range.stop.second %>% filter(country.code %in% over.countries)
  
  data.range.stop.second$index <- as.numeric(row.names(data.range.stop.second))
  print(data.range.stop.second)
  data.range.stop.second$index <- 1700-50*data.range.stop.second$index
  
  data.range.stop.second <-
    data.range.stop.second %>% mutate(OC221 = ifelse(country.code %in% over.countries, 550, OC221)) %>% mutate(OC222_322 =                                                                                        ifelse(country.code %in%over.countries, index, OC222_322))
  
  print(data.range.stop.second)
  
  
  
  p <- ggplot(data, aes(OC221, OC222_322, group = legend.name))
  p <- p +
    geom_point(data = data.range.start,
               color = 'black',
               size = 1) +
    
    geom_point(
      data = data.range.stop,
      color = 'black',
      size = 2,
      shape = 21
    ) +
    geom_point(
      data = data.range.start,
      color = 'black',
      size = 1,
      shape = 1
    ) +
    theme_bw() +    ylab("Nurses & Midwives per 100.000") +
    xlab("Doctors  per 100.000") +  labs(color = paste(sprintf('\u2022'), 'from ', sprintf('\u25e6'), 'to')) +
    
    geom_path(aes(color = text.plot)) + #theme(legend.position = "bottom") +
    
    
    # add only for countries level, not for NUTS2
    geom_text(
      data = data.range.stop,
      aes(label = text.plot, colour = factor(geo.name)),
      hjust = 1,
      nudge_x = 0,
      vjust = -1,
      nudge_y = 0,
      size = 2,
      check_overlap = TRUE
    ) +
    geom_text(
      data = data.range.stop.second,
      aes(label = text.plot, colour = factor(geo.name)),
      hjust = 0,
      nudge_x = 0,
      vjust = 0,
      nudge_y = 0,
      size = 2,
      check_overlap = FALSE
    ) +
    
    theme(legend.position = "none")
  
  
  ggsave(paste0('plots/','all_countries', '.png'), p)
  return(p)
}


plot.density.scatter.timeline.region <- function(data,code.country) {
  data <- data %>% filter(country.code==code.country)
  
  data.range.start <- data %>%
    group_by(geo) %>%
    arrange(year) %>%
    filter(row_number() == 1)
  
  data.range.stop <- data %>%
    group_by(geo) %>%
    arrange(year) %>%
    filter(row_number() == n())
  
  
  p <- ggplot(data, aes(OC221, OC222_322, group = legend.name))
  p <- p +
    geom_point(data = data.range.start,
               color = 'black',
               size = 1) +
    
    geom_point(
      data = data.range.stop,
      color = 'black',
      size = 2,
      shape = 21
    ) +
    geom_point(
      data = data.range.start,
      color = 'black',
      size = 1,
      shape = 1
    ) +
    theme_bw() +    ylab("Nurses & Midwives per 100.000") +
    xlab("Doctors  per 100.000") +  labs(color = paste(sprintf('\u2022'), 'from ', sprintf('\u25e6'), 'to')) +
    
    geom_path(aes(color = text.plot)) +#theme(legend.position = "bottom") +
    
    
    
    theme(legend.text = element_text(size = 4))
  
  
  ggsave(paste0('plots/',code.country, '.png'), p)
  return(p)
}