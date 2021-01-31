#' Visualizing Maximum Likelihood Estimation Shiny Application
#'
#' This will start a shiny app explore maximum likelihood estimation visually.
#'
#' @references http://rstudio.com/shiny
#' @export
shiny_mle <- function() {
	shiny::runApp(system.file('shiny', package='visualMLE'))
}
