% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_lat_lon_BNG.R
\name{os_lat_lon_to_grid}
\alias{os_lat_lon_to_grid}
\title{Convert Latitude and Longitude to Ordnance Survey Grid Reference}
\usage{
os_lat_lon_to_grid(lat, lon)
}
\arguments{
\item{lat}{numeric vector of latitude coordinates}

\item{lon}{numeric vector of longitude coordinates}
}
\value{
A list with a string containing the Ordnance Survey grid reference for
the lat-lon coordinate pair.
}
\description{
This function uses the 'OSGridConverter' Python package to convert latitude and
longitude coordinates to Ordnance Survey grid references.
}
\examples{

os_lat_lon_to_grid(-2.353, 51.509)

}
\seealso{
\code{\link{os_grid_to_lat_lon}}
}
