#' Plot OS Land Cover
#'
#' Generate an interactive map of Ordnance Survey (OS) Land Cover data around a specified coordinate using the OS Data Hub API.
#'
#' @param lon numeric value indicating the longitude of the required point (e.g. -0.1426)
#' @param lat numeric value indicating the latitude of the required point (e.g. 51.5244)
#' @param key character value for the OS Data Hub API key in use
#' @param buff numeric value indicating the buffer around the specified point in metres (default = 1000)
#' @param n integer value indicating the maximum number of results to return (default = 10000)
#'
#' @return returns a list including the following objects:
#' \itemize{
#'  \item\code{sf}: an sf object containing the queried data,
#'  \item\code{map}: an interactive map generated using the queried data,
#'  \item\code{tiers}: an sf object containing the tier B land cover information.}
#'
#' @details Requires the OS Data Hub API key and needed packages to run (tidyverse, reticulate, mapview, sf, tictoc, colourspace, leaflet).
#'
#' @import needs
#' @import tidyverse
#' @import reticulate
#' @import mapview
#' @import sf
#' @import tictoc
#' @import colourvalues
#' @import leaflet
#' @import colorspace
#' @importFrom stringr str_detect
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom dplyr filter
#'
#' @examples
#'
#' # Generate a plot of land cover around the London Eye
#' key <- "insert OS Data Hub API key"
#' plot_os_land_cover(lon = -0.1194, lat = 51.5033, key = key)
#'
#' @export

plot_os_land_cover <- function(lon, lat, key, buff = 1000, n = 10000L){

  # lon <- lon
  # lat <- lat

  ## load packages
  require(needs)

  needs(tidyverse, reticulate, mapview, sf, tictoc, colourvalues, leaflet, colorspace)

  ## create virtual environment (this will need to be relative path)
  Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/tinyforest/bin/python")
  use_virtualenv("tinyforest")
  #py_install("osdatahub", pip = TRUE, envname = "tinyforest")

  key = key
  ## import modules

  osmaps <- import("osdatahub")
  Extent <- osmaps$Extent
  geojson <- import("geojson")
  places <- osmaps$PlacesAPI

  ## get os collections

  #os_col <- osmaps$NGD$get_collections()
  # point <- c(lon, lat)


  collection = "lnd-fts-land-1"
  ngd = osmaps$NGD(key, collection)

  lon <- lon
  lat <- lat

  x1 <- lon - buff
  x2 <- lon + buff
  y1 <- lat - buff
  y2 <- lat + buff

  bbox <- c(x1, y1, x2, y2)


  extent = Extent$from_bbox(bbox, crs = "EPSG:27700")

  tic()
  results = ngd$query(max_results = n, extent)
  toc()

  sf::sf_use_s2(FALSE)

  gj <- geojson$dumps(results, sort_keys = TRUE) |>
    sf::read_sf()

  tierb <- gj |>
    unnest("oslandcovertierb") |>
    select(oslandcovertierb) |>
    filter(!str_detect(oslandcovertierb, "Made|Under"))

  pal <- colorspace::diverging_hcl(palette = "Green-Brown", rev = TRUE, n = length(unique(tierb$oslandcovertierb)))

  mv <- tierb  |>
    #count(oslandcovertiera, oslandcovertierb) |>
    #unnest("oslandcovertierb")
    mapview(zcol = "oslandcovertierb", col.region = pal)

  out <- list(sf = gj, map = mv, tiers = tierb)

}





