
#' Initialize TinyForest package virtual environment
#'
#' This function initializes the TinyForest environment with required dependencies and libraries.
#'
#' @import reticulate
#' @import dplyr
#'
#' @return No explicit output, but this function creates and configures the required environment for TinyForest
#'
#' @examples
#' initialize_tf()
#'
#' @export



initialize_tf <- function(){

  require(reticulate); require(dplyr)

  virtualenv_remove("tinyforest")
  virtualenv_create("tinyforest")
  Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/tinyforest/bin/python")
  use_virtualenv("tinyforest")

  py_install(c("earthengine-api", "geemap", "osdatahub"),  pip = TRUE, envname = "tinyforest")

  ee <- import("ee")
  geemap <- import("geemap")
  os <- import("osdatahub")





}


