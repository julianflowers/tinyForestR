#' Calculate normalized difference vegetation index (NDVI) within a buffer around a point location
#'
#' This function takes in a latitude, longitude, buffer distance, start and end time period, and cloud cover percentage,
#' and returns a map of NDVI within the buffer region, along with statistics on the NDVI values within the buffer.
#'
#' @param lat Numeric. The latitude of the point location of interest.
#' @param lon Numeric. The longitude of the point location of interest.
#' @param dist Numeric. The distance in meters to draw a buffer around the point location.
#' @param start_date Character. The start date in the format 'YYYY-MM-DD' for the time period of interest.
#' @param end_date Character. The end date in the format 'YYYY-MM-DD' for the time period of interest.
#' @param cloud_cover Numeric. The maximum allowable percentage of cloud cover in the satellite imagery used to calculate the NDVI. Default is 10.
#'
#' @return A list object containing the following components:
#' map: A map with two layers displaying the NDVI and the input point location.
#' tidy_dates: A tidied version of the image collection with start and end dates associated with each image in the collection#' image_ids: A vector of image IDs corresponding to the images used in the calculation.
#' raster: A raster layer object of the NDVI values within the buffer region.
#' ndvi_stats: Summary statistics of the NDVI values within the buffer region, including median, minimum, maximum, mean and standard deviation.
#'
#' @import reticulate rgee tidyrgee zoo terra
#' @export
#'
#' @examples
#' calc_ndvi_buff(lat = 25.1972, lon = 55.2744, dist = 500, start_date = '2021-01-01',
#' end_date = '2021-12-31',cloud_cover = 10)
#'
#' @references
#' - COPERNICUS/S2_SR_HARMONIZED image collection.
#' - rgee package documentation: https://csaybar.github.io/rgee/index.html
#' - tidyrgee package documentation: https://csaybar.github.io/tidyrgee/index.html
#' - zoo package documentation: https://cran.r-project.org/web/packages/zoo/index.html
#' - terra package documentation: https://rspatial.org/terr'



calc_ndvi_buff <- function(lat = 25.1972, lon = 55.2744, dist = 500, start_date = "2021-01-01", end_date = "2021-12-31", cloud_cover = 10){

  require(reticulate); require(rgee); require(tidyrgee); if(!require(zoo))install.packages("zoo")
  library(zoo)
  require(terra)

  ee <- import("ee")
  geemap <- import("geemap")
  geedim <- import("geedim")

  #ee_Authenticate()
  ee_Initialize(drive = TRUE)

  ## load image collection

  ic <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")

  ## parameters

  lon <- lon
  lat <- lat


  point <- ee$Geometry$Point(c(lon, lat))
  buff <- ee$Geometry$Point(c(lon, lat))$buffer(dist)
  start <- start_date
  end <- end_date
  ccover <- cloud_cover

  ## image collection filters
  s2 <- ic$filterBounds(buff)
  s2 <- s2$filterDate(start, end)
  s2 <- s2$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', ccover))

  pal <- c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301', "#000000")

  palette <- colorRampPalette(pal)

  raster <- ee_as_raster(image = s2$median()$clip(buff)$normalizedDifference(c("B8","B4")), region = buff, scale = 1, via = "drive"
               )

  ndvi_df <- raster |>
    terra::as.data.frame() |>
    drop_na()

  ndvi_stats <- raster |>
    terra::as.data.frame() |>
    drop_na() |>
    summarise(med = median(nd),
              min = min(nd),
              max = max(nd),
              mean = mean(nd),
              sd = sd(nd))


  Map$setCenter(lon =lon, lat = lat, zoom = 15)
  map <- Map$addLayer(s2$median()$clip(buff)$normalizedDifference(c("B8","B4")),
               visParams = list(min = -0.5, max = 1, palette = palette(15)), name = "NDVI") +
    Map$addLayer(point, name = "TF")

  s2_tidy <- tidyrgee::as_tidyee(s2)

  out <- list(map = map, tidy_dates = s2_tidy, image_ids = s2_tidy$vrt |>
                pluck("id"), raster = raster, ndvi_stats = ndvi_stats, ndvi_df = ndvi_df)


}

