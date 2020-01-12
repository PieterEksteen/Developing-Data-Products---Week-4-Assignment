library(fst)
library(dplyr)
library(prophet)
library(ggplot2)
library(plotly)

options("scipen" = 999,"digits"=5)

##--Not in select fuction
'%!in%' <- function(x, y)
    ! ('%in%'(x, y))

install.packages("jsonlite", repos="https://cran.rstudio.com/")
##--Not in select fuction
'%!in%' <- function(x, y)
    ! ('%in%'(x, y))library("jsonlite")

##--------------------------------------------------------------------------------------------------
##--Get Population Data
##--------------------------------------------------------------------------------------------------

json_file <- 'https://datahub.io/core/population/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
    if(json_data$resources$datahub$type[i]=='derived/csv'){
        path_to_file = json_data$resources$path[i]
        data <- read.csv(url(path_to_file))
        print(data)
    }
}

write.fst(data, file.path(here::here(), 'Developing Data Products', 'world_population_shiny_data.fst'), 100)

population_data <- read.fst(file.path(here::here(), 'Developing Data Products', 'world_population_shiny_data.fst'))

world_pop_2016 <- population_data %>% 
    filter(Year == 2016, 
           Country.Code == 'WLD')

exclude_codes <- c('ARB',
                   'CSS',
                   'CEB',
                   'EAR',
                   'EAS',
                   'EAP',
                   'TEA',
                   'EMU',
                   'ECS',
                   'ECA',
                   'TEC',
                   'EUU',
                   'FCS',
                   'HPC',
                   'HIC',
                   'IBD',
                   'IBT',
                   'IDB',
                   'IDX',
                   'IDA',
                   'LTE',
                   'LCN',
                   'LAC',
                   'TLA',
                   'LDC',
                   'LMY',
                   'MEA',
                   'TMN',
                   'MIC',
                   'NAC',
                   'UMC',
                   'PSE',
                   'UMC',
                   'SSF',
                   'SSA',
                   'TSS',
                   'OED',
                   'OSS',
                   'PSS',
                   'PST',
                   'PRE',
                   'SST',
                   'SAS',
                   'LIC',
                   'LMC',
                   'MNA',
                   'TSA')

population_data <- population_data %>% 
    filter(Country.Code %!in% exclude_codes) %>% 
    mutate(Country.Name = as.character(Country.Name),
           Country.Name = ifelse(Country.Name == 'World', 'All Countries', Country.Name))

write.fst(population_data, file.path(here::here(), 'Developing Data Products', 'world_population_shiny_data_cleaned.fst'), 100)

##--------------------------------------------------------------------------------------------------
##--Get Geo Data
##--------------------------------------------------------------------------------------------------

##--Get Countries Geo data
library(XML)
library(leaflet)

site <- "http://www.csgnetwork.com/llinfotable.html"
countries_geo_table <- readHTMLTable(site, header=T, which=6,stringsAsFactors=F)

library(stringr)

test <- countries_geo_table %>% 
    mutate(Latitude = str_replace_all(Latitude, 'Â', ''),
           Longitude = str_replace_all(Longitude, 'Â', '')) 

library(sp)

chd = substr(test$Latitude, 3, 3)[1]
chm = substr(test$Latitude, 6, 6)[1]
chs = substr(test$Latitude, 6, 6)[1]

##--Latitude
cd = char2dms(test$Latitude,chd=chd,chm=chm)

Latitude = as.numeric(cd)

##--Longitude
cd = char2dms(test$Longitude,chd=chd,chm=chm)

Longitude = as.numeric(cd)

test$Latitude <- Latitude
test$Longitude <- Longitude

map <- test %>% 
    leaflet(width = '100%') %>% 
    addTiles() %>%
    addProviderTiles("OpenMapSurfer.Roads") %>%
    addCircleMarkers(lng = test$Longitude,
                     lat = test$Latitude,
                     weight = 2,
                     # radius = sqrt(cities_data$Population)/100,
                     # color = cities_data$color_hex,
                     # fillColor = cities_data$color_hex,
                     popup = test$Country,
                     clusterOptions = markerClusterOptions())  

map

# ##--Load Population Data
# population_data <- read.fst(file.path(here::here(),
#                                       'Developing Data Products',
#                                       'world_population_shiny_data_prepared.fst'))

##--Test Join
join_test <- anti_join(population_data, test, by = c('Country.Name' = 'Country'))

pop_countries_not_joined <- unique(join_test$Country.Name)

join_test2 <- anti_join(test, population_data, by = c('Country' = 'Country.Name'))

test_countries_not_joined <- unique(join_test2$Country)

##--Apply Fixes
population_data2 <- population_data %>% 
    mutate(Country.Name = case_when(Country.Name == 'Bahamas, The' ~ 'Bahamas',
                                    Country.Name == 'Cabo Verde' ~ 'Cape Verde',
                                    Country.Name == 'Congo, Rep.' ~ 'Republic of the Congo',
                                    Country.Name == 'Korea, Dem. Peopleâ€™s Rep.' ~ "Democratic People's Republic of Korea",
                                    Country.Name == 'Congo, Dem. Rep.' ~ 'Democratic Republic of the Congo',
                                    Country.Name == 'Egypt, Arab Rep.' ~ 'Egypt',
                                    Country.Name == 'Gambia, The' ~ 'Gambia',
                                    Country.Name == 'Iran, Islamic Rep.' ~ 'Islamic Republic of Iran',
                                    Country.Name == 'Kyrgyz Republic' ~ 'Kyrgyzstan',
                                    Country.Name == 'Lao PDR' ~ "Lao People's Democratic Republic",
                                    Country.Name == 'Macao SAR, China' ~ 'Macao, China',
                                    Country.Name == 'Micronesia, Fed. Sts.' ~ 'Micronesia (Federated States of)',
                                    Country.Name == 'Korea, Rep.' ~ 'Republic of Korea',
                                    Country.Name == 'St. Kitts and Nevis' ~ 'Saint Kitts and Nevis',
                                    Country.Name == 'St. Lucia' ~ 'Saint Lucia',
                                    Country.Name == 'St. Vincent and the Grenadines' ~ 'Saint Vincent and the Grenadines',
                                    Country.Name == 'United States' ~ 'United States of America',
                                    Country.Name == 'Venezuela, RB' ~ 'Venezuela',
                                    TRUE ~ Country.Name
    ))



test2 <- test %>% 
    mutate(Country = case_when(Country == 'American  Samoa' ~ 'American Samoa',
                               Country == 'Comros' ~ 'Comoros',
                               Country == 'Congo' ~ 'Republic of the Congo',
                               Country == "Democratic People's Republic of" ~ "Democratic People's Republic of Korea",
                               Country == 'Dominica Republic' ~ 'Dominican Republic',
                               Country == 'Iran (Islamic Republic of)' ~ 'Islamic Republic of Iran',
                               Country == "Lao People's  Democratic Republic" ~ "Lao People's Democratic Republic",
                               Country == 'Libyan Arab Jamahiriya' ~ 'Libya',
                               Country == 'Moldova, Republic of' ~ 'Moldova',
                               Country == 'Republic of  Korea' ~ 'Republic of Korea',
                               Country == 'Rawanda' ~ 'Rwanda',
                               Country == 'Saint vincent and the Grenadines' ~ 'Saint Vincent and the Grenadines',
                               Country == 'The Former Yugoslav Republic of Macedonia' ~ 'Macedonia, FYR',
                               Country == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
                               Country == 'United  Republic of Tanzania' ~ 'Tanzania',
                               Country == 'United States\r\n  of America' ~ 'United States of America',
                               Country == 'United States\r\n  of Virgin Islands' ~ 'Virgin Islands (U.S.)',
                               Country == 'Viet Nam' ~ 'Vietnam',
                               TRUE ~ Country
    ))

##--Test Join
join_test <- anti_join(population_data2, test2, by = c('Country.Name' = 'Country'))

pop_countries_not_joined <- unique(join_test$Country.Name)

join_test2 <- anti_join(test2, population_data2, by = c('Country' = 'Country.Name'))

test_countries_not_joined <- unique(join_test2$Country)

##--Join and keep only countries with both population and geo data
pop_data_complete <- left_join(test2, population_data2, by = c('Country' = 'Country.Name'))

pop_data_complete <- pop_data_complete %>% 
    select(-Country.Code, -Capital)

##--Extract world population data
world_pop <- population_data2 %>% 
    filter(Country.Name == 'All Countries') %>% 
    mutate(Latitude = 0,
           Longitude = 0) %>%
    rename(Country = Country.Name) %>% 
    select(Country, Latitude, Longitude, Year, Value)

pop_data_complete <- rbind(world_pop,
                           pop_data_complete) %>% 
    filter(!is.na(Value))

##--Add Colors for Population size
pop_data_complete <- pop_data_complete %>% 
    mutate(pop_color = case_when(Value < 5000000 ~ 'red',
                                 Value >= 5000000 & Value < 10000000 ~ 'orange',
                                 Value >= 10000000 & Value < 20000000 ~ 'yellow',
                                 Value >= 20000000 & Value < 50000000 ~ 'lightblue',
                                 Value >= 50000000 & Value < 100000000 ~ 'darkblue',
                                 Value >= 100000000 & Value < 350000000 ~ '#20B2AA',
                                 Value > 1000000000 ~ 'darkgreen'))

debug_mode <- FALSE

if(debug_mode) {
    
    map_data <- pop_data_complete %>% 
        filter(Year == max(Year),
               Country != 'All Countries')
    
    map <- map_data %>% 
        leaflet(
            # height = '100%'
        ) %>% 
        addTiles() %>%
        addProviderTiles("OpenMapSurfer.Roads") %>%
        addCircleMarkers(lng = map_data$Longitude,
                         lat = map_data$Latitude,
                         weight = 2,
                         radius = sqrt(map_data$Value)/500,
                         color = map_data$pop_color,
                         fillColor = map_data$pop_color,
                         popup = paste('Country:', 
                                       toupper(map_data$Country),
                                       '<br>',
                                       'Year:',
                                       map_data$Year,
                                       '<br>',
                                       'Population:',
                                       format(map_data$Value, big.mark = "'"))) %>% 
        setView(lat = 0, lng = 0, zoom = 2.5) %>% 
    addLegend(title = 'Population',
              labels = c('Less Than 5M',
                         'Between 5M and 10M',
                         'Between 10M and 20M',
                         'Between 20M and 50M',
                         'Between 50M and 100M',
                         'Between 100M and 350M',
                         'More than 1B'),
              colors = c('red',
                         'orange',
                         'yellow',
                         'lightblue',
                         'darkblue',
                         '#20B2AA',
                         'darkgreen'),
              position = 'topright')
    
    map
    
}


write.fst(pop_data_complete, file.path(here::here(), 'Developing Data Products', 'world_population_shiny_data_prepared.fst'), 100)

