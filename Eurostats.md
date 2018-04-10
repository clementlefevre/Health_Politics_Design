R Notebook
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(ggplot2)
library(eurostat)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(stringr)
library('rgdal')      # Lire et reprojeter les cartes
```

    ## Loading required package: sp

    ## rgdal: version: 1.2-15, (SVN revision 691)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.2.2, released 2017/09/15
    ##  Path to GDAL shared files: /usr/share/gdal/2.2
    ##  GDAL binary built with GEOS: TRUE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.2, 08 September 2015, [PJ_VERSION: 492]
    ##  Path to PROJ.4 shared files: (autodetected)
    ##  Linking to sp version: 1.2-4

``` r
library('plotrix')    # Créer des échelles de couleurs
library('classInt')   # Affecter ces couleurs aux données
library('Cairo')
library('sf')
```

    ## Linking to GEOS 3.5.1, GDAL 2.2.2, proj.4 4.9.2

``` r
library(rgeos)  # for gIntersection
```

    ## rgeos version: 0.3-26, (SVN revision 560)
    ##  GEOS runtime version: 3.5.1-CAPI-1.9.1 r4246 
    ##  Linking to sp version: 1.2-4 
    ##  Polygon checking: TRUE

``` r
library(raster)
```

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

Load World Map, Bosnia and Oceans
=================================

``` r
world.map <- readOGR(dsn="eurostats_data/ne_10m_admin_0_countries/",layer="ne_10m_admin_0_countries")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "eurostats_data/ne_10m_admin_0_countries/", layer: "ne_10m_admin_0_countries"
    ## with 255 features
    ## It has 71 fields

``` r
world.map <- world.map[world.map$CONTINENT %in% c('Europe','Africa','Asia'),]

#world.map <- gSimplify(world.map, tol = 0.00002)
world.map <- spTransform(world.map, CRS("+init=epsg:2154"))

bosnia.map <- readOGR(dsn="eurostats_data/BIH_adm_shp/",layer="BIH_adm1")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "eurostats_data/BIH_adm_shp/", layer: "BIH_adm1"
    ## with 3 features
    ## It has 12 fields
    ## Integer64 fields read as strings:  ID_0 ID_1 CCN_1

``` r
bosnia.map <- aggregate(bosnia.map)
bosnia.map <- spTransform(bosnia.map, CRS("+init=epsg:2154"))

ocean <- readOGR(dsn="eurostats_data/ne_50m_ocean/", layer="ne_50m_ocean")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "eurostats_data/ne_50m_ocean/", layer: "ne_50m_ocean"
    ## with 1 features
    ## It has 3 fields
    ## Integer64 fields read as strings:  scalerank

``` r
ocean <- spTransform(ocean, CRS("+init=epsg:2154"))
```

### Load eurostats DATASET

``` r
dataset<- 'hlth_rs_prsrg'

df.data <- get_eurostat('hlth_rs_prsrg')
```

    ## Table hlth_rs_prsrg cached at /tmp/Rtmpbk5beb/eurostat/hlth_rs_prsrg_date_code_TF.rds

``` r
df.data <- df.data %>% filter(unit=='P_HTHAB')#%>% filter(str_length(geo)>2)
dput(df.data,'hlth_rs_prsrg.put')
hlth_rs_prsrg<- dget('hlth_rs_prsrg.put')
df.labels <-  (label_eurostat(df.data,lang='en',fix_duplicated = TRUE)) %>% dplyr::select(isco08,geo)
```

    ## Labels for geo includes duplicated labels in the Eurostat dictionary. Codes have been added as a prefix for dublicated.
    ## Modified labels are: LU Luxembourg, LU00 Luxembourg, MT Malta, MT00 Malta, LI Liechtenstein, LI00 Liechtenstein

``` r
colnames(df.labels)<- c('profession','region')

df.data<- tibble::rowid_to_column(df.data, "ID")
df.labels<- tibble::rowid_to_column(df.labels, "ID")

df<- merge(df.data,df.labels,by='ID',all=TRUE)

dput(df,'hlth_rs_prsrg.dput')
df$year<- year(df$time)
df$time<-NULL
df$ID<-NULL
```

Define quartiles for each profession
------------------------------------

``` r
str(df)
```

    ## 'data.frame':    20630 obs. of  7 variables:
    ##  $ unit      : Factor w/ 3 levels "HAB_P","NR","P_HTHAB": 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ isco08    : Factor w/ 5 levels "OC221","OC222_322",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ geo       : Factor w/ 298 levels "IT","ITC1","ITC2",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ values    : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ profession: Factor w/ 5 levels "Medical doctors",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ region    : Factor w/ 298 levels "Italy","Piemonte",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ year      : num  2016 2016 2016 2016 2016 ...

``` r
ggplot(df,aes(profession,values))+ geom_boxplot()
```

    ## Warning: Removed 243 rows containing non-finite values (stat_boxplot).

![](Eurostats_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

``` r
ggplot(df, aes(x=values, fill=profession)) +
    geom_histogram(binwidth=.5, alpha=.5, position="identity")
```

    ## Warning: Removed 243 rows containing non-finite values (stat_bin).

![](Eurostats_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-2.png)

``` r
quartiles <- df %>% filter(!is.na(values)) %>% group_by(profession) %>% summarise(q0=quantile(values,c(0)),
                                                                     q1=quantile(values,c(0.25)),
                                                                     q2=quantile(values,c(0.5)),
                                                                     q3=quantile(values,c(0.75)))

quartiles
```

    ## # A tibble: 5 x 5
    ##            profession    q0       q1     q2       q3
    ##                <fctr> <dbl>    <dbl>  <dbl>    <dbl>
    ## 1     Medical doctors 80.75 253.7350 314.74 376.7025
    ## 2 Nurses and midwives 34.17 506.7200 652.38 939.2700
    ## 3            Dentists  4.02  41.6250  59.11  77.8725
    ## 4         Pharmacists  3.93  45.0100  66.06 104.3225
    ## 5    Physiotherapists  0.09  21.3925  61.33 111.4425

``` r
quartiles.list <- list('Medical doctors'= c(80,100,250,320,380,600,1000),
'Nurses and midwives'=c(30,500,650,1000,1500),
'Dentists' = c(4,40,60,75),
'Pharmacists' =  c(4,45,65,100),
'Physiotherapists' = c(0.01,20,60,100))
```

Set Date ranges
---------------

``` r
dates_range <- unique(df$year)
professions <- unique(df$profession)
date.range <- c(2000, 2005, 2010, 2014,2015)
```

### Function to prepare data for a given profession

``` r
create.data.per.profession <- function(df,prof){
  
  df.europe.year <- df %>% filter(profession==prof) %>% filter(year %in% date.range)  %>% filter(!geo=='EU28')
  df.europe.year$country <- str_sub(df.europe.year$geo,1,2)
  df.europe.year$geo.level <-str_length(df.europe.year$geo)-1
  df.europe.year <- df.europe.year %>% spread(year,values)
  
  ## Define the geo level for each country
  geo.level.df <- df.europe.year %>% group_by(country,geo.level) %>% summarise(count = n())
  number.geo.level <- sort(unique(geo.level.df$geo.level))
  print(str(number.geo.level))
  geo.level.df <- geo.level.df %>% spread(geo.level,count)
  print (head(geo.level.df))
  
  if(identical(number.geo.level,c(1,2,3))){
    print ('coucou 1')
    colnames(geo.level.df)<- c('country','level.1','level.2','level.3')
    geo.level.df <- geo.level.df %>% mutate('lowest.geo.level'=ifelse(!is.na(level.3),3,ifelse(!is.na(level.2),2,1)))
  
  } else if(identical(number.geo.level,c(1,2))){
     print ('coucou 2')
     colnames(geo.level.df)<- c('country','level.1','level.2')
    geo.level.df <- geo.level.df %>% mutate('lowest.geo.level'=ifelse(!is.na(level.2),2,1))
  
} else if(identical(number.geo.level,c(1,3))){
   print ('coucou 3')
     colnames(geo.level.df)<- c('country','level.1','level.3')
    geo.level.df <- geo.level.df %>% mutate('lowest.geo.level'=ifelse(!is.na(level.3),3,1))
  }
  
  
  ## Filter data on the lowest geo level available
  df.europe.year <- merge(df.europe.year,geo.level.df%>% dplyr::select(country,lowest.geo.level),by='country')
  df.europe.year <- df.europe.year %>% filter(geo.level==lowest.geo.level)
  print (head(df.europe.year))
  colnames(df.europe.year) <- make.names(colnames(df.europe.year), unique = TRUE)
  
  return (df.europe.year)
}

df.test <- create.data.per.profession(df,professions[3])
```

    ##  num [1:3] 1 2 3
    ## NULL
    ## # A tibble: 6 x 4
    ## # Groups:   country [6]
    ##   country   `1`   `2`   `3`
    ##     <chr> <int> <int> <int>
    ## 1      AL     1    NA    NA
    ## 2      AT     1    NA     9
    ## 3      BE     1    NA    11
    ## 4      BG     1    NA     6
    ## 5      CH     1    NA     7
    ## 6      CY     1    NA     1
    ## [1] "coucou 1"
    ##   country    unit isco08  geo profession           region geo.level  2000
    ## 1      AL P_HTHAB OC2261   AL   Dentists          Albania         1 47.60
    ## 2      AT P_HTHAB OC2261 AT11   Dentists  Burgenland (AT)         3 29.34
    ## 3      AT P_HTHAB OC2261 AT12   Dentists Niederösterreich         3 38.71
    ## 4      AT P_HTHAB OC2261 AT13   Dentists             Wien         3 68.91
    ## 5      AT P_HTHAB OC2261 AT21   Dentists          Kärnten         3 41.60
    ## 6      AT P_HTHAB OC2261 AT22   Dentists       Steiermark         3 44.22
    ##    2005  2010  2014  2015 lowest.geo.level
    ## 1    NA    NA    NA    NA                1
    ## 2 34.46 37.97 40.66 41.81                3
    ## 3 44.13 42.05 45.13 44.77                3
    ## 4 70.68 79.17 79.77 76.81                3
    ## 5 48.47 54.05 56.08 56.41                3
    ## 6 48.21 53.58 54.45 54.99                3

Rename Countries to match with World Map
========================================

``` r
EU_NUTS <- readOGR(dsn = "eurostats_data/NUTS_2010_60M_SH/Data", layer = "NUTS_RG_60M_2010")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "eurostats_data/NUTS_2010_60M_SH/Data", layer: "NUTS_RG_60M_2010"
    ## with 1920 features
    ## It has 4 fields

``` r
proj4string(EU_NUTS)
```

    ## [1] "+proj=longlat +ellps=GRS80 +no_defs"

``` r
EU_NUTS <- spTransform(EU_NUTS, CRS("+init=epsg:2154"))
df.EU_NUTS <- data.frame(EU_NUTS@data)
EU.ISO <- unique(str_sub(df.EU_NUTS$NUTS_ID,1,2))
#EU.ISO <- replace(EU.ISO, EU.ISO=="EL",'GR')
EU.ISO <- replace(EU.ISO, EU.ISO=="UK",'GB')
EU.ISO <- replace(EU.ISO, EU.ISO=="PT",'PR')
EU.ISO <- replace(EU.ISO, EU.ISO=="AT",'AU')
EU.ISO <- replace(EU.ISO, EU.ISO=="PL",'PO')
EU.ISO <- replace(EU.ISO, EU.ISO=="IE",'IR')
EU.ISO <- replace(EU.ISO, EU.ISO=="DK",'DN')
EU.ISO <- replace(EU.ISO, EU.ISO=="SE",'SW')
```

Function to ploat a given year/profession
-----------------------------------------

``` r
plot_map<- function(df,year,profession,breaks){
  
  breaks <- quartiles.list[[profession]]
  print (length(breaks))
  year.col <-paste('X',as.character(year),sep='')
  print(year.col)
  end_color= '#31a354'
  start_color='#e5f5e0'
  
  df.europe.year <- create.data.per.profession(df,profession)
  
    EU_NUTS_with_Data <- merge(EU_NUTS,df.europe.year,by.x='NUTS_ID',by.y='geo',all.x=TRUE, duplicateGeoms = TRUE)
    EU_NUTS_with_Data <- EU_NUTS_with_Data[!is.na(EU_NUTS_with_Data$geo.level),]
    
    EU_NUTS_with_Data <- EU_NUTS_with_Data[!is.na(EU_NUTS_with_Data@data[,year.col]),]
    
    df.EU_NUTS_with_Data <- data.frame(EU_NUTS_with_Data@data)
    
    
    col <- findColours(classIntervals(
              EU_NUTS_with_Data@data[,year.col], length(breaks)-1, style="fixed",fixedBreaks=breaks),
              smoothColors(start_color,98,end_color))
    # Légende
    leg <- findColours(classIntervals(
              round(EU_NUTS_with_Data@data[,year.col]),  length(breaks)-1, style="fixed",fixedBreaks=breaks),
              smoothColors(start_color,7,end_color),
              under="<", over=">", between="–",
              cutlabels=FALSE)
    
    # define map limits
    xlim = c(-828843  ,4287547)
    ylim = c(5107843  ,9602614)
    CP <- as(extent(c(xlim,ylim)), "SpatialPolygons")
    
    proj4string(CP) <- proj4string(EU_NUTS_with_Data)
    CP <- spTransform(CP, CRS("+init=epsg:2154"))
    
    title <- paste(profession,as.character(year))
    #png(file =paste(title,'.png',sep=''), w = 1800, h = 1800, res = 300)
    plot.new()
    
    
    EU_NUTS_with_Data <- crop(EU_NUTS_with_Data, extent(CP))
    EU_NUTS <- crop(EU_NUTS, extent(CP))
    
    ocean.cropped <- gSimplify(ocean, tol = 0.00001)
    
    # this is a well known R / GEOS hack (usually combined with the above) to 
    # deal with "bad" polygons
    ocean.cropped <- gBuffer(ocean.cropped, byid=TRUE, width=0)
    ocean.cropped <- crop(ocean.cropped, extent(CP))
    
    world.map <- gBuffer(world.map, byid=TRUE, width=0)
    world.map <- crop(world.map, extent(CP))
    
    
    plot(world.map,lwd=.1,col='lightgrey',main=title)
    plot(EU_NUTS,col='white',lwd=.1,add=TRUE)
    plot(ocean.cropped,col='lightblue',lwd=0.1,add=TRUE) 
    plot(bosnia.map,lwd=0.1,col='lightgrey',add=TRUE)
    plot(EU_NUTS_with_Data,col=col,lwd=.1,add=TRUE)
    
    legend(2362890,9348213,fill=c('white',attr(leg, "palette")),
     legend=c('No Data',names(attr(leg,"table"))),
      title = 'per 100 000 Habitants :',cex = .5)
    
    ##dev.off()
  
}
```

Tests
-----

``` r
plot_map(df,2015,professions[1])
```

    ## [1] 7
    ## [1] "X2015"
    ##  num [1:3] 1 2 3
    ## NULL
    ## # A tibble: 6 x 4
    ## # Groups:   country [6]
    ##   country   `1`   `2`   `3`
    ##     <chr> <int> <int> <int>
    ## 1      AL     1    NA    NA
    ## 2      AT     1    NA     9
    ## 3      BE     1    NA    11
    ## 4      BG     1    NA     6
    ## 5      CH     1    NA     7
    ## 6      CY     1    NA     1
    ## [1] "coucou 1"
    ##   country    unit isco08  geo      profession           region geo.level
    ## 1      AL P_HTHAB  OC221   AL Medical doctors          Albania         1
    ## 2      AT P_HTHAB  OC221 AT11 Medical doctors  Burgenland (AT)         3
    ## 3      AT P_HTHAB  OC221 AT12 Medical doctors Niederösterreich         3
    ## 4      AT P_HTHAB  OC221 AT13 Medical doctors             Wien         3
    ## 5      AT P_HTHAB  OC221 AT21 Medical doctors          Kärnten         3
    ## 6      AT P_HTHAB  OC221 AT22 Medical doctors       Steiermark         3
    ##     2000   2005   2010   2014   2015 lowest.geo.level
    ## 1 141.30     NA     NA     NA     NA                1
    ## 2 269.48 305.84 351.93 377.76 387.71                3
    ## 3 316.08 379.11 428.83 458.31 456.63                3
    ## 4 605.45 625.26 661.13 689.25 684.98                3
    ## 5 332.42 363.08 413.93 448.24 455.19                3
    ## 6 366.37 412.66 462.59 485.58 499.83                3

![](Eurostats_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-9-1.png)

``` r
# for(prof in professions){
#   for(year in date.range){
#     plot_map(df,year,prof)
#   }
# }
```
