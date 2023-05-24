#' Get NDVI time series
#'
#' This function retrieves the time series of Normalized Difference Vegetation Index (NDVI) values
#' at a specified location and within a given time frame. The function returns a list containing the
#' summary statistics of NDVI values at the specified point and a visualization of the time series.
#'
#' @param lon numeric: Longitude of the location of interest.
#' @param lat numeric: Latitude of the location of interest.
#' @param start character: Start date of the time frame, in YYYY-MM-DD format (default is '2019-01-01').
#' @param end character: End date of the time frame, in YYYY-MM-DD format (default is '2023-01-01').
#' @param ic character: Google Earth Engine ImageCollection name (default is "COPERNICUS/S2_SR").
#' @param buffer1 numeric: Buffer around point of interest, in meters (default is 100).
#' @param buffer2 numeric: Buffer around point of interest to retrieve summary statistics, in meters (default is 1000).
#'
#' @return list containing:
#' \describe{
#'  \item{summary_point}{Summary statistics of NDVI values at the specified point#'  \item{sd}{Standard deviation of NDVI values at the specified point}
#'  \item{summary_buff}{Summary statistics of NDVI values within buffer2 around the specified point}
#'  \item{sd_buff}{Standard deviation of NDVI values within buffer2 around the specified point}
#'  \item{plot}{Visualization of the time series of NDVI values at the specified point}
#' }
#'
#' @examples
#' get_ndvi_time_series(-121.7, 36.9)
#'
#' @importFrom tictoc tic toc
#' @importFrom purrr map map_dfr pluck
#' @importFrom ggplot2 ggplot aes geom_point geom_line
#'
#' @export


get_ndvi_time_series  <- function(lon, lat, start = "2019-01-01", end = "2023-01-01", ic = "COPERNICUS/S2_SR", buffer1 = 50, buffer2 = 1000){

  lon <- lon
  lat <- lat
  ic <- ic
  start <- start
  end <- end

  b1 <- buffer1
  b2 <- buffer2

  point <- ee$Geometry$Point(c(lon, lat))$buffer(buffer1)
  bounds <- ee$Geometry$Point(c(lon, lat))$buffer(buffer2)

  # start <- "2019-01-01"
  # end <- "2023-01-01"

  addNDVI <- function(image){
    return(image$addBands(image$normalizedDifference(c('B8','B4'))$rename("ndvi")))
  }

  sent <- ee$ImageCollection(ic)
  sent1 <- sent$filterDate(start, end)
  sent1 <- sent1$filterBounds(bounds)
  sent1 <- sent1$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 20))

  ndvi <- sent1$map(addNDVI)

  ic_d2 <- ee_get_date_ic(sent1)

  ids <- pluck(ic_d2, "id")
  dates <- pluck(ic_d2, "time_start")

  images <- map(ids, ee$Image)
  ndvi <- map(images, addNDVI)

  tictoc::tic()
  ndvi_summary <- purrr::map_dfr(ndvi, ~ee_extract(.x, y = point, fun = ee$Reducer$median(), sf = FALSE))
  ndvi_sd <- purrr::map_dfr(ndvi, ~ee_extract(.x, y = point, fun = ee$Reducer$stdDev(), sf = FALSE))
  ndvi_summary_b2 <- purrr::map_dfr(ndvi, ~ee_extract(.x, y = bounds, fun = ee$Reducer$median(), sf = FALSE))
  ndvi_sd_b2 <- purrr::map_dfr(ndvi, ~ee_extract(.x, y = bounds, fun = ee$Reducer$stdDev(), sf = FALSE))
  tictoc::toc()

  plot <- ndvi_summary |>
    bind_cols(date=dates) |>
    ggplot(aes(date, ndvi)) +
    geom_point() +
    geom_line(lty = "dotted")

  out <- list(dates = dates, summary_point = ndvi_summary, sd = ndvi_sd, summary_buff = ndvi_summary_b2, sd_buff = ndvi_sd_b2, plot = plot)

}




