#' Save ggmap Images
#'
#' This function saves a ggmap satellite image given a longitude and latitude.
#'
#' @param lon longitude of the center of the image
#' @param lat latitude of the center of the image
#' @param key Google API key for ggmap imagery
#'
#' @import ggmap
#' @import tidyverse
#' @import sf
#' @import here
#' @import lubridate
#' @import vegan
#' @import data.table
#' @import mapview
#'
#' @examples
#' save_ggmap_images(-71.0636, 42.3581, "API_key")
#'
#' @export



save_ggmap_images<- function(lon, lat, key){

  Sys.setenv(GGMAP_GOOGLE_API_KEY= key)
  library(needs)
  needs(ggmap, tidyverse, sf, here, lubridate, vegan, data.table, mapview)
  ggmap::register_google(key)
  ggmap::get_googlemap(center = c(lon = lon, lat = lat),
                       zoom = 19, maptype = "satellite") |>
  ggmap() -> p


  ggsave(paste0(here::here(), "/images/tf_", lon, lat, ".png"), p)

}


