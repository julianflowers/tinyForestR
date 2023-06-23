' Get species counts from NBN Atlas records based on location and taxa
#'
#' This function takes a longitude, latitude, taxa, and optional search radius and returns a data frame with the species, count, and other information for records found on the NBN Atlas.
#'
#' @param lon Longitude of centre point for search
#' @param lat Latitude of centre point for search
#' @param taxa Scientific or common name of taxa to search for
#' @param radius Search radius in kilometres around the centre point, defaults to 1 km
#'
#' @return A data frame of species counts with columns for scientific name, common name, count, and other information.
#'
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' get_species_counts( -1.394457, 50.936542, "Bombus terrestris" )
#'
#' @export


get_species_counts <- function(lon, lat, taxa, radius = 1){

  base_url <- "https://records-ws.nbnatlas.org/explore/group/"
  search_url <- glue::glue(base_url, taxa, "?lat=", {lat}, "&lon=", {lon}, "&radius=", {radius} )

  out <- jsonlite::fromJSON(search_url, simplifyDataFrame=TRUE)

  return(out)

}
