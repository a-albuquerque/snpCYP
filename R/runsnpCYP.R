#' This will launch Shiny implementation of snpCYP
#'
#' A function to run Shiny application for snpCYP
#'
#' @return No return value, should open up a Shiny webpage.
#'
#' @examples
#' \dontrun{
#' snpCYP::runsnpCYP()
#' }
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials.
#'     \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp

runsnpCYP <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "snpCYP")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
# [END]
