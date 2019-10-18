
# devtools::install_github("ropensci/rnoaa")

library(rnoaa)

chicago_station <- "GHCND:US1ILCK0145"

NOAA_KEY <- "hYDbNtXOsizrXxbEcBmXdMQtRHRAbJQm"

ncdc(datasetid = 'GHCND', stationid = 'GHCND:USW00014895', startdate = '2013-10-01',
     enddate = '2013-12-01')



test_res <- ncdc(datasetid = 'GHCNDMS', stationid = 'GHCND:US1ILCK0012', startdate = '2016-02-01',
     enddate = '2016-03-01', limit = 1000)

