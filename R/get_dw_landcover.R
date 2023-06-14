#' Get land cover data from the Dynamic World Land Cover dataset for a given location
#'
#' Downloads the image collection from the Dynamic World Land Cover (DW) dataset on Google Earth Engine (GEE) platform, filters it using a given location and time frame, and returns the land cover statistics for the given area.
#'
#' @param tf text: specify which TerraFusion site ID to use, defaults to tf_id
#' @param lon numeric: longitude of the center of the area of interest, defaults to 0.0969
#' @param lat numeric: latitude of the center of the area of interest, defaults to 51.578
#' @param dist numeric: distance around the center point to create a circular buffer in meters, defaults to 500
#' @param start_date character: starting date of the image collection, defaults to '2022-01-01'
#' @param end_date character: ending date of the image collection, defaults to '2022-12-31'
#' @return list: a list containing the following items:
#' \item{map}{a map object showing the image collection and area of interest}
#' \item{image_dates}{vector of image dates in the collection}
#' \item{#' image_ids}{vector of image IDs in the collection}
#' \item{raster}{a cropped raster object showing the land cover statistics for the area of interest}
#' \item{lc_stats}{a data frame containing the land cover class counts for the area of interest}
#' \item{dw_df}{a data frame containing all the land cover data for the area of interest}
#' \item{tf_id}{the TerraFusion site ID used}
#' \item{buffer}{the buffer distance used to select the area of interest}
#'
#' @import reticulate
#' @import rgee
#' @import tidyrgee
#' @importFrom tidyr drop_na
#' @importFrom terra as.data.frame
#' @importFrom terra as_raster
#' @importFrom graphics plot
#' @importFrom dplyr count
#' @importFrom leaflet addLayer
#' @importFrom leaflet setCenter
#' @importFrom purrr pluck
#'
#' #' @examples
#' \dontrun{
#' # Get data for the default location
#' get_dw_landcover()
#'
#' # Get data for a specific location and buffer distance
#' get_dw_landcover(tf = "JPL-1", lon = -118.2426, lat = 34.0522, dist = 1000)
#' }
#'
#' @export


get_dw_landcover <- function(tf = tf_id, lon = 0.0969, lat = 51.578, dist = 500, start_date = "2022-01-01", end_date = "2022-12-31"){

  require(reticulate); require(rgee); require(tidyrgee); if(!require(zoo))install.packages("zoo")
  library(zoo)
  require(terra)
  initialise_tf()

  ee <- import("ee")
  geemap <- import("geemap")
  geedim <- import("geedim")

  #ee_Authenticate()
  ee_Initialize(drive = TRUE)

  ## load image collection

  ic <- ee$ImageCollection("GOOGLE/DYNAMICWORLD/V1")

  ## parameters

  lon <- lon
  lat <- lat


  point <- ee$Geometry$Point(c(lon, lat))
  buff <- ee$Geometry$Point(c(lon, lat))$buffer(dist)
  start <- start_date
  end <- end_date
  #ccover <- cloud_cover

  ## image collection filters
  s2 <- ic$filterBounds(buff)
  s2 <- s2$filterDate(start, end)
  s2 <- s2$select("label")
  #s2 <- s2$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', ccover))

  pal <- c(
    '#419bdf', '#397d49', '#88b053', '#7a87c6', '#e49653', '#dfc35a','#c42811',
    '#a59b8f', '#b39fe1')

  #palette <- colorRampPalette(pal)

  raster <- ee_as_raster(image = s2$mode()$clip(buff), region = buff, scale = 1, via = "drive")

  plot(raster, col = pal)


  dw_df <- raster |>
    terra::as.data.frame() |>
    drop_na()

  lc_stats <- raster |>
    terra::as.data.frame() |>
    drop_na() |>
    count(label)


  Map$setCenter(lon =lon, lat = lat, zoom = 15)
  map <- Map$addLayer(s2$median()$clip(buff),
                      visParams = list(min = 0, max = 8, palette = pal), name = "DW", opacity = 0.6) +
    Map$addLayer(point, name = "TF")

  s2_tidy <- tidyrgee::as_tidyee(s2)

  out <- list(map = map, image_dates = s2_tidy$vrt$date, image_ids = s2_tidy$vrt$id |>
                pluck("id"), raster = raster, lc_stats = lc_stats, dw_df = dw_df, tf_id = tf, buffer = dist)


}

