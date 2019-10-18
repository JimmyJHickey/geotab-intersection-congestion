
library(riem)
library(drake)
library(dplyr)

# load dependencies
loadd(train_Atlanta)
loadd(train_Boston)
loadd(train_Chicago)
loadd(train_Philadelphia)

loadd(train_normalize_census)
loadd(test_normalize_census)

# returns the station id of the weather station closest to the intersections in the city

closest_weather_station <- function(city_dat, network) {
  
  med_lat <- median(city_dat$Latitude)
  med_lon <- median(city_dat$Longitude)
  
  city_stations <- riem_stations(network = network)
  
  distances <- sqrt((city_stations$lat - med_lat)^2 + (city_stations$lon - med_lon)^2)
  
  station_id <- city_stations$id[which.min(distances)]
  
}



# res <- riem_measures("ORD", date_start = "2018-06-01")

merge_weather_plan <- drake_plan(

  station_id_Atlanta = closest_weather_station(train_Atlanta, "GA_ASOS"),
  station_id_Boston = closest_weather_station(train_Boston, "MA_ASOS"),
  station_id_Chicago = closest_weather_station(train_Chicago, "IL_ASOS"),
  station_id_Philadelphia = closest_weather_station(train_Philadelphia, "PA_ASOS"),
  
  # getting weather data
  weather_Atlanta = riem_measures(station_id_Atlanta, date_start = "2018-05-01", date_end = "2019-01-31"),
  weather_Boston = riem_measures(station_id_Boston, date_start = "2018-05-01", date_end = "2019-01-31"),
  weather_Chicago = riem_measures(station_id_Chicago, date_start = "2018-05-01", date_end = "2019-01-31"),
  weather_Philadelphia = riem_measures(station_id_Philadelphia, date_start = "2018-05-01", date_end = "2019-01-31")
  
)



config <- drake_config(merge_weather_plan)

vis_drake_graph(config)



make(merge_weather_plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()



