
library(riem)
library(drake)
library(dplyr)
library(lubridate)
library(fastDummies)

# load dependencies
train_Atlanta <- readd(train_Atlanta)
train_Boston <- readd(train_Boston)
train_Chicago <- readd(train_Chicago)
train_Philadelphia <- readd(train_Philadelphia)

train_normalize_census <- readd(train_normalize_census)
test_normalize_census <- readd(test_normalize_census)



# returns the station id of the weather station closest to the intersections in the city

closest_weather_station <- function(city_dat, network) {
  
  med_lat <- median(city_dat$Latitude)
  med_lon <- median(city_dat$Longitude)
  
  city_stations <- riem_stations(network = network)
  
  if (city_dat$City[1] == "Chicago") {
    # this weather station doesn't have any up-to-date data
    city_stations <- city_stations[city_stations$id != "CGX",]
  }
  
  distances <- sqrt((city_stations$lat - med_lat)^2 + (city_stations$lon - med_lon)^2)
  
  station_id <- city_stations$id[which.min(distances)]
  
}




# helper function to find the minute of the hourly measurements for each city's weather station

reference_minutes <- function(weather_cities) {
  
  ref_mins <- c()
  
  for (i in 1:length(weather_cities)) {
    
    # extract minutes for which tmpf measurements aren't  NA
    mins <- minute(weather_cities[[i]]$valid[!is.na(weather_cities[[i]]$tmpf)]) 
    
    ref_min <- as.integer(names(sort(table(mins), decreasing = TRUE)[1]))
    
    ref_mins[i] <- ref_min
    
  }
  
  return(ref_mins)
  
}



# helper function to aggregate the weather data by hour, month, and city, using summarise()

weather_aggregate <- function(weather_cities, ref_mins) {
  
  # extract weather data only for the reference minutes
  # weather_cities_filter <- lapply(weather_cities, function(x) x[minute(x$valid) == ref_mins[which(names(weather_cities) )],])
  
  weather_cities_filtered <- list()
  
  for (i in 1:length(weather_cities)) {
    
    weather_city <- weather_cities[[i]]
    
    weather_city_filtered <- weather_city[minute(weather_city$valid) == ref_mins[i],]
    
    weather_cities_filtered[[i]] <- weather_city_filtered
    
  }
  
  weather_all <- do.call("rbind.data.frame", weather_cities_filtered)
  
  weather_all <- mutate(weather_all, Hour = hour(weather_all$valid), 
                        Month = month(weather_all$valid))
  
  # turning factor variable sky into 6 dummy variables
  
  weather_all <- dummy_cols(weather_all, select_columns = "skyc1")
  
  # summarizing variables that have less than or equal to 1000 NAs
  
  weather_agg <- weather_all %>%
    group_by(Hour, Month, station) %>%
    summarise(tmpf = mean(tmpf, na.rm = TRUE), dwpf = mean(dwpf, na.rm = TRUE), relh = mean(relh, na.rm = TRUE),
              drct = mean(drct, na.rm = TRUE), sknt = mean(sknt, na.rm = TRUE), p01i = mean(p01i, na.rm = TRUE),
              alti = mean(alti, na.rm = TRUE), mslp = mean(mslp, na.rm = TRUE), vsby = mean(vsby, na.rm = TRUE),
              feel = mean(feel, na.rm = TRUE), skyc1_CLR = mean(skyc1_CLR, na.rm = TRUE), 
              skyc1_FEW = mean(skyc1_FEW, na.rm = TRUE), skyc1_SCT = mean(skyc1_SCT, na.rm = TRUE),
              skyc1_BKN = mean(skyc1_BKN, na.rm = TRUE), skyc1_OVC = mean(skyc1_OVC, na.rm = TRUE),
              skyc1_VV = mean(`skyc1_VV `, na.rm = TRUE))
  
}



# extract weather data for each combination of hour, month, and city (hmc) from weather_city and merge it into dat, 
# then return the result
# weather_cities is the list of weather_city for each city. Assumes each object in list weather_cities has the name of 
# the city it's referring to

append_weather <- function(dat, weather_cities, station_ids) {
  
  hour_month_city <- paste(dat$Hour, dat$Month, dat$City)
  
  hour_month_city_unique <- unique(hour_month_city)
  
  ref_mins <- reference_minutes(weather_cities)
  
  weather_agg <- weather_aggregate(weather_cities, ref_mins)
  
  # initialize new variables for dataframe, and naming them at the same time
  num_rows <- dim(dat)[1]
  
  dat$Temperature <- rep(NA, num_rows)
  dat$DewPointTemperature <- rep(NA, num_rows)
  dat$Humidity <- rep(NA, num_rows)
  dat$WindDirection <- rep(NA, num_rows)
  dat$WindSpeed <- rep(NA, num_rows)
  dat$Precipitation <- rep(NA, num_rows)
  dat$PressureAltitude <- rep(NA, num_rows)
  dat$SeaLevelPressure <- rep(NA, num_rows)
  dat$Visibility <- rep(NA, num_rows)
  dat$FeelsLikeTemperature <- rep(NA, num_rows)
  dat$SkyClear <- rep(NA, num_rows)
  dat$SkyFew <- rep(NA, num_rows)
  dat$SkyScattered <- rep(NA, num_rows)
  dat$SkyBroken <- rep(NA, num_rows)
  dat$SkyOvercast <- rep(NA, num_rows) 
  dat$SkyVV <- rep(NA, num_rows) # I'm actually not sure what VV stands for
  
  for (hmc in hour_month_city_unique) {
    
    hour <- as.integer(strsplit(hmc, " ")[[1]][1])
    month <- as.integer(strsplit(hmc, " ")[[1]][2])
    city <- strsplit(hmc, " ")[[1]][3]
    
    station <- station_ids[[which(names(station_ids) == city)]]
    
    weather_hmc <- weather_agg[weather_agg$Hour == hour & weather_agg$Month == month &
                                 weather_agg$station == station,]
    
    dat$Temperature[hour_month_city == hmc] <- weather_hmc$tmpf
    dat$DewPointTemperature[hour_month_city == hmc] <- weather_hmc$dwpf
    dat$Humidity[hour_month_city == hmc] <- weather_hmc$relh
    dat$WindDirection[hour_month_city == hmc] <- weather_hmc$drct
    dat$WindSpeed[hour_month_city == hmc] <- weather_hmc$sknt
    dat$Precipitation[hour_month_city == hmc] <- weather_hmc$p01i
    dat$PressureAltitude[hour_month_city == hmc] <- weather_hmc$alti
    dat$SeaLevelPressure[hour_month_city == hmc] <- weather_hmc$mslp
    dat$Visibility[hour_month_city == hmc] <- weather_hmc$vsby
    dat$FeelsLikeTemperature[hour_month_city == hmc] <- weather_hmc$feel
    dat$SkyClear[hour_month_city == hmc] <- weather_hmc$skyc1_CLR
    dat$SkyFew[hour_month_city == hmc] <- weather_hmc$skyc1_FEW
    dat$SkyScattered[hour_month_city == hmc] <- weather_hmc$skyc1_SCT
    dat$SkyBroken[hour_month_city == hmc] <- weather_hmc$skyc1_BKN
    dat$SkyOvercast[hour_month_city == hmc] <- weather_hmc$skyc1_OVC
    dat$SkyVV[hour_month_city == hmc] <- weather_hmc$skyc1_VV
    
  }
  
  return(dat)
  
}



merge_weather_plan <- drake_plan(

  station_id_Atlanta = closest_weather_station(train_Atlanta, "GA_ASOS"),
  station_id_Boston = closest_weather_station(train_Boston, "MA_ASOS"),
  station_id_Chicago = closest_weather_station(train_Chicago, "IL_ASOS"),
  station_id_Philadelphia = closest_weather_station(train_Philadelphia, "PA_ASOS"),
  
  # getting weather data
  weather_Atlanta = riem_measures(station_id_Atlanta, date_start = "2018-05-01", date_end = "2019-01-31"),
  weather_Boston = riem_measures(station_id_Boston, date_start = "2018-05-01", date_end = "2019-01-31"),
  weather_Chicago = riem_measures(station_id_Chicago, date_start = "2018-05-01", date_end = "2019-01-31"),
  weather_Philadelphia = riem_measures(station_id_Philadelphia, date_start = "2018-05-01", date_end = "2019-01-31"),
  
  # extract weather data for each combination of hour, month, and city (hmc) from weather_city and merge it into train_normalize_census and test_normalize_census
  train_append_weather = append_weather(train_normalize_census, weather_cities = list(Atlanta = weather_Atlanta, Boston = weather_Boston, 
                                                                                      Chicago = weather_Chicago, Philadelphia = weather_Philadelphia),
                                                                station_ids = list(Atlanta = station_id_Atlanta, Boston = station_id_Boston, 
                                                                                   Chicago = station_id_Chicago, Philadelphia = station_id_Philadelphia)),
  test_append_weather = append_weather(test_normalize_census, weather_cities = list(Atlanta = weather_Atlanta, Boston = weather_Boston, 
                                                                                     Chicago = weather_Chicago, Philadelphia = weather_Philadelphia),
                                       station_ids = list(Atlanta = station_id_Atlanta, Boston = station_id_Boston, 
                                                          Chicago = station_id_Chicago, Philadelphia = station_id_Philadelphia))
  
)



config <- drake_config(merge_weather_plan)

vis_drake_graph(config)



make(merge_weather_plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()



# save final data in convenient rds/RData

loadd(train_append_weather)

save(train_append_weather, file = "backup_data_files/train_append_weather.RData")

write.csv(train_append_weather, file = "backup_data_files/train_append_weather.csv")



loadd(test_append_weather)

save(test_append_weather, file = "backup_data_files/test_append_weather.RData")

write.csv(test_append_weather, file = "backup_data_files/test_append_weather.csv")





