#' Extracts NDVI from Google Earth Engine
#'
#' @param lat numeric Latitude of the region of interest
#' @param lon numeric Longitude of the region of interest
#' @param start_date character Start date of the time series in yyyy-mm-dd format
#' @param end_date character End date of the time series in yyyy-mm-dd format
#' @param x character User-defined identifier used to label the output file
#' @return NULL
#' @import lubridate tidyverse
#' @importFrom geemap create_timeseries download_ee_image
#' @importFrom ee Geometry Point addBands normalizedDifference qualityMosaic
#' @export



extract_ndvi <- function(lat, lon, start_date, end_date, x) {
    require(lubridate); require(tidyverse)

    ee <- import("ee")
    geemap <- import('geemap')
    geedim <- import("geedim")

    ee$Initialize()

    start_date <- "2022-01-01"
    end_date <- "2023-01-01"
    x <- 85
    point <- ee$Geometry$Point(c(lon, lat))
    buff <- ee$Geometry$Point(c(lon, lat))$buffer(1000)

    addNDVI <- function(image){
      return(image$addBands(image$normalizedDifference(c('B8','B4'))$rename("ndvi")))
    }

    g_ic <- geemap$create_timeseries("COPERNICUS/S2_SR", start_date = start_date, end_date = end_date,
                                     region = buff,
                                     bands = c("B8", "B4", "B3", "B2"),
                                     frequency = 'month', reducer = 'median')

    ndvi <- g_ic$map(addNDVI)

    green <- ndvi$qualityMosaic('ndvi')

    geemap$download_ee_image(image = green, filename = paste0("green_", x, "_", start_date, ".tif"),
                             region = buff, scale = 1, crs = "EPSG:4326", num_threads = 6)

}

test <- extract_ndvi(lat = 56, lon = -2, start_date = "2022-01-01", end_date = "2023-01-01", x = 85)

raster::stack("green_85_2022-01-01.tif") |>
  plot()


