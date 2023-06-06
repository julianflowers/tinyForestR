#' NBN Atlas data for 1km buffer round each Tiny Forest location 2015-2023: birds
#'
#' A subset of data from the NBN Atlas dataset
#'
#'
#' @format ## `birds_filt`
#' A data frame with 309,090 rows and 15 columns:
#' \describe{
#'   \item{kingdom}{Animal kingdowm}
#'   \item{phylum, classs, order, family, genus}{taxomomic classification}
#'   \item{decimalLatitude, decimalLongitude}{decimal coordinates}
#'   \item{year, month}{Year and month of collection}
#'   \item{dataProviderName}{data source}
#'   \item{speciesGrouns, vernacularName, species}{species common and scientific name}
#'   \item{tf_id}{tinyforest id}

#'   ...
#' }
#'
#'
#' #' NBN Atlas data for 1km buffer round each Tiny Forest location 2015-2023: flowering plants
#'
#' A subset of data from the NBN Atlas dataset
#'
#'
#' @format ## `flowers_filt`
#' A data frame with 23,143 rows and 15 columns:
#' \describe{
#'   \item{kingdom}{Animal kingdowm}
#'   \item{phylum, classs, order, family, genus}{taxomomic classification}
#'   \item{decimalLatitude, decimalLongitude}{decimal coordinates}
#'   \item{year, month}{Year and month of collection}
#'   \item{dataProviderName}{data source}
#'   \item{speciesGrouns, vernacularName, species}{species common and scientific name}
#'   \item{tf_id}{tinyforest id}

#'   ...
#' }

#' NBN Atlas data for 1km buffer round each Tiny Forest location 2015-2023: insects
#'
#' A subset of data from the NBN Atlas dataset
#'
#'
#' @format ## `insects_filt`
#' A data frame with 45,597 rows and 15 columns:
#' \describe{
#'   \item{kingdom}{Animal kingdowm}
#'   \item{phylum, classs, order, family, genus}{taxomomic classification}
#'   \item{decimalLatitude, decimalLongitude}{decimal coordinates}
#'   \item{year, month}{Year and month of collection}
#'   \item{dataProviderName}{data source}
#'   \item{speciesGrouns, vernacularName, species}{species common and scientific name}
#'   \item{tf_id}{tinyforest id}

#'   ...
#' }
#'

#' @title Tiny Forest (TF) data
#' @description TF ids, planting dates, area, lat-lon and tree species planted
#' @format A data frame with 190 rows and 7 variables:
#' \describe{
#'   \item{\code{tf_id}}{double Tiny Forest ID}
#'   \item{\code{plant_date}}{character Date planted}
#'   \item{\code{area}}{double Area in m2}
#'   \item{\code{trees}}{list Tree species planted}
#'   \item{\code{lat}}{double Decimal latitude}
#'   \item{\code{lon}}{double Decimal longitude}
#'   \item{\code{date}}{double Date planted}
#'}
#' @source \url{https://tinyforest.earthwatch.org.uk/tiny-forest-sites}
"tf_data"
