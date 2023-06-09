#' Convert Latitude and Longitude to Ordnance Survey Grid Reference
#'
#' This function uses the 'OSGridConverter' Python package to convert latitude and
#' longitude coordinates to Ordnance Survey grid references.
#'
#' @param lon numeric vector of longitude coordinates
#' @param lat numeric vector of latitude coordinates
#' @return A list with a string containing the Ordnance Survey grid reference for
#' the lat-lon coordinate pair.
#' @import reticulate
#' @import stringr
#' @examples
#'
#' os_lat_lon_to_grid(-2.353, 51.509)
#'
#' @seealso
#'
#' \code{\link{os_grid_to_lat_lon}}
#'
#' @export


os_lat_lon_to_grid <- function(lat, lon){

  require(reticulate); require(tidyverse)

  Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/tinyforest/bin/python")
  use_virtualenv("tinyforest")

  py_install("OSGridConverter", pip = TRUE, envname = "tinyforest")

  osgrid <- import("OSGridConverter")

 ll <- osgrid$grid2latlong("TL4871")
  grid <- osgrid$latlong2grid(lat, lon)
  r <- py_to_r(grid) |>
    as.character()

  py_to_r(ll) |>
    as.character()

  r <- paste0(stringr::str_sub(r[1], 1, 2), stringr::str_sub(r[1], 4,7))
  out <- list(grid = r)

}

r <- paste0(stringr::str_sub(ll[1], 1, 2), stringr::str_sub(ll[1], 4,7))

lon <- 0.1767
lat <- 52.3172

point <- data.frame(lon, lat) |> st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(27700)
buffer <- st_buffer(point, 1000)

get_nbn_buffer(lon, lat) |>
  filter(str_detect(species, "Strept")) |>
  select(species, contains("decimal"), year) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  st_transform(27700) |>
  mapview(zcol = "year") +
  mapview(buffer)
