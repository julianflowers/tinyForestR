#' Calculate normalized difference vegetation index (NDVI) within a buffer around a point location
#'
#' This function takes in a latitude, longitude, buffer distance, start and end time period, and cloud cover percentage,
#' and returns a map of NDVI within the buffer region, along with statistics on the NDVI values within the buffer.
#'
#' @param dist1 A numeric indicating the buffer distance in meters around the point of interest. Default is 1000.
#' @param dist2 A numeric indicating the buffer distance in meters around the point used to filter the image collection. Default is 50.
#' @param start_date A character indicating the start date of the image collection in the format 'YYYY-MM-DD'. Default is '2021-01-01'.
#' @param end_date A character indicating the end date of the image collection in the format 'YYYY-MM-DD'. Default is '2021-12-31'.
#' @param cloud_cover A numeric indicating the maximum percentage of cloudy pixels allowed in the image collection. Default is 10.
#' @param tf_id An optional#' parameter indicating an ID or name for the point of interest. Default is NULL.
#'
#'
#' @return A list object containing the following components:
#' map: A map with two layers displaying the NDVI and the input point location.
#' tidy_dates: A tidied version of the image collection with start and end dates associated with each image in the collection#' image_ids: A vector of image IDs corresponding to the images used in the calculation.
#' raster: A raster layer object of the NDVI values within the buffer region.
#' ndvi_stats: Summary statistics of the NDVI values within the buffer region, including median, minimum, maximum, mean and standard deviation.
#'
#' @importFrom reticulate import
#' @importFrom rgee ee_Initialize
#' @importFrom tidyrgee as_tidyee ee_extract_tidy
#' @importFrom zoo zoo
#' @import terra
#'
#' @export
#'
##' @examples
#'
#' # Calculate NDVI and its summary statistics within a buffer and a point of interest for default parameters
#' calc_ndvi_buff()
#'
#' @seealso
#' \code{\link{as_tidyee}}, \code{\link{ee_extract_tidy}}
#'
#' @rdname calc_ndvi_buff
#'
#' @references
#' - COPERNICUS/S2_SR_HARMONIZED image collection.
#' - rgee package documentation: https://csaybar.github.io/rgee/index.html
#' - tidyrgee package documentation: https://csaybar.github.io/tidyrgee/index.html
#' - zoo package documentation: https://cran.r-project.org/web/packages/zoo/index.html
#' - terra package documentation: https://rspatial.org/terr'


# Define a function to calculate NDVI from satellite imagery using certain parameters

calc_ndvi_buff <- function(lon = -1.469, lat = 51.7779, dist1 = 1000, dist2 = 50, start_date = "2019-01-01", end_date = "2022-12-31", cloud_cover = 10, tf_id = 85){

  # Load necessary packages

  require(reticulate); require(rgee); require(tidyrgee); if(!require(zoo))install.packages("zoo")
  library(zoo)
  require(terra)
  library(lubridate)

  # Import Google Earth Engine Python module
  ee <- import("ee")

  # Import geemap package for visualization
  geemap <- import("geemap")

  # Import geedim package for Earth Engine data extraction
  geedim <- import("geedim")

  #ee_Authenticate()

  # Initialize Google Earth Engine
  ee_Initialize(drive = TRUE)

  ## load image collection

  ic <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")

  ## parameters

  lon <- lon
  lat <- lat

  # Define time range and cloud cover threshold for image collection filtering
  point <- ee$Geometry$Point(c(lon, lat))$buffer(dist2)
  buff <- ee$Geometry$Point(c(lon, lat))$buffer(dist1)
  start <- start_date
  end <- end_date
  ccover <- cloud_cover

  ## image collection filters
  s2 <- ic$filterBounds(buff)
  s2 <- s2$filterDate(start, end)
  s2 <- s2$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', ccover))


  # Function to add NDVI band to an image

  addNDVI <- function(image){
    return(image$addBands(image$normalizedDifference(c('B8','B4'))$rename("ndvi")))
  }

  # Define color palette for NDVI visualization
  pal <- c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301', "#000000")


  # Apply the addNDVI function to each image in the image collection
  ndvi <- s2$map(addNDVI)

  # Extract the median, mean, and standard deviation of NDVI for each month from 2019 to 2023 at the defined point location
  ndvi_yr_m <- ndvi |>
    tidyrgee::as_tidyee() |>
    select("ndvi") |>
    filter(year %in% year(start):year(end)) |>
    group_by(year, month) |>
    summarise(stat=c("median", "mean", "sd")) |>
    mutate(date = lubridate::make_date(year, month, day = 1L))


  # Extract the NDVI value at the defined point location for each month from 2019 to 2023
  point_summary <- ndvi_yr_m |>
    ee_extract_tidy(y = point,
                    fun = "median",
                    scale = 10) |>
    mutate(tf_id = tf_id) |>
    pivot_wider(names_from = "parameter", values_from = "value")

  # Extract the median NDVI for each month from 2019 to 2023 within the defined buffer area
  buffer_summary <- ndvi_yr_m |>
    ee_extract_tidy(y = buff,
                    fun = "median",
                    scale = 10) |>
    mutate(tf_id = tf_id) |>
    pivot_wider(names_from = "parameter", values_from = "value")


  Map$setCenter(lon =lon, lat = lat, zoom = 15)
  map <- Map$addLayer(s2$median()$clip(buff)$normalizedDifference(c("B8","B4")),
                      visParams = list(min = -0.5, max = 1, palette = pal), name = paste(start_date, end_date)) +
    Map$addLayer(point, visParams = list(min = -0.5, max = 1, palette = pal), name = "TF")


  # Map$setCenter(lon =lon, lat = lat, zoom = 15)
  # map <- Map$addLayer(s2$median()$clip(buff)$normalizedDifference(c("B8","B4")),
  #              visParams = list(min = -0.5, max = 1, palette = palette(15)), name = "NDVI") +
  #   Map$addLayer(point, name = "TF")
  #
  # s2_tidy <- tidyrgee::as_tidyee(s2)

  # Create output object containing NDVI summary statistics for the defined point and buffer locations, plot, and tidy NDVI data
  out <- list(map = map, point_summary = point_summary,  buffer_summary= buffer_summary,
              tidy_ndvi = ndvi_yr_m$vrt, tf_id = tf_id)

}

