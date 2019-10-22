
library(broom)

data <- df.nuts2 %>% gather(OC221,OC222_322,key = isco08, value = density) %>%drop_na() 
data$geo.name<-as.factor(data$geo.name)
df.model.n.samples<-  data %>%  group_by(country.code,isco08,year) %>% summarise(observations=n()) 

cols.predictors <- c( 'HAB_KM2',  'T_Y_GE75',  'T_Y_LT5', 'T_Y10.14', 'T_Y15.19', 'T_Y20.24', 'T_Y25.29', 'T_Y30.34', 'T_Y35.39', 'T_Y40.44', 'T_Y45.49', 'T_Y5.9', 'T_Y50.54', 'T_Y55.59', 'T_Y60.64', 'T_Y65.69', 'T_Y70.74', 'T_Y75.79', 'T_Y80.84', 'EUR_HAB')
formula <- paste('density','~',paste0(cols.predictors,collapse ='+'))

df.model.rsquared <-data %>% 
  group_by(country.code,isco08) %>% 
  do(glance(lm(formula, data = . ))) 

df.model.coef <-data %>% drop_na()  %>% 
  group_by(country.code,isco08) %>% 
  do(tidy(lm(formula, data = .)))   #%>% slice(2)

df.model <- merge(df.model.n.samples,df.model.rsquared,by=c('country.code','isco08','year'))
df.model <- merge(df.model,df.model.coef,by=c('country.code','isco08','year'),suffixes=c(".r2", ".coef"))

write.xlsx(df.model,'linear_model_nurses_phys_pop_density.xlsx')


