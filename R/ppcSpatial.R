#'@title Spatial Analysis of Pakistan Population Census 2017
#'
#'@description GUI for Pakistan Population Census 2017.
#'
#'@import shiny
#'@export

Launch_ppcSpatial <- function() {
  runApp(
        appDir = system.file(
                      "ppcSpatialShinyApp"
                     , package = "ppcSpatial"
                     )
      , launch.browser = TRUE
      )

}
