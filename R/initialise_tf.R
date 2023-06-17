
#' Initialize TinyForest package virtual environment
#'
#' This function initializes the TinyForest environment with required dependencies and libraries.
#'
#' @import reticulate
#' @import dplyr
#'
#' @return No explicit output, but this function creates and configures the required environment for TinyForest.
#'
#' @examples
#' initialize_tf()
#'
#' @export



initialise_tf <- function(){

  require(reticulate); require(dplyr)
  library(needs)
  needs(tidyverse)


  ## create virtual envinronent called `tinyforest`
  #virtualenv_remove("tinyforest")
  virtualenv_create("tinyforest")
  Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/tinyforest/bin/python")
  use_virtualenv("tinyforest")

  py_install(c("earthengine-api", "geemap", "osdatahub", "OSGridConverter", "geedim", "plotly"),  pip = TRUE, envname = "tinyforest")

  ee <- import("ee")
  geemap <- import("geemap")
  os <- import("osdatahub")
  osgrid <- import("OSGridConverter")
  geedim <- import("geedim")

}


