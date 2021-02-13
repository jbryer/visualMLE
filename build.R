library(devtools)

#Package building
devtools::document()
devtools::install(force = TRUE, dependencies = FALSE)
devtools::install(build_vignettes = TRUE, dependencies = FALSE)
devtools::build()
devtools::clean_vignettes()
devtools::build_vignettes()
devtools::check(cran=TRUE)
devtools::release()

visualMLE::shiny_mle()
visualMLE::shiny_mle(df = iris)
visualMLE::shiny_mle(df = iris, default_x = 'Sepal.Length', default_y = 'Petal.Length')
vignette('mle', package = 'visualMLE')


#' Save the Shiny App to an app.R file.
#'
#' This function will create three files in the \code{out_dir}: \code{server.R},
#' \code{ui.R}, and \code{global.R}. The contents of \code{server.R} and
#' \code{ui.R} will be the source code of the \code{server_function} and
#' \code{ui_function}, respectively. The \code{global.R} file will only contain
#' \code{library} calls for \code{shiny} and any other packages specified in
#' the \code{pkgs} parameter.
#'
#' If \code{run_app = TRUE} the function will start the Shiny app once the
#' files are written. This is recommended to ensure all the necessary packages
#' are loaded for the Shiny app to run.
#'
#' @param ui_function the function for the UI.
#' @param server_function the function for the server.
#' @param pkgs any packages that need to be loaded for the app to work. At
#'        minimum the package containing the shiny app should be included.
#' @param out_dir the directory to save the shiny app files.
#' @param run_app whether to run the app once the files are saved.
save_shiny_app <- function(ui_function,
						   server_function,
						   pkgs,
						   out_dir = 'shiny',
						   run_app = TRUE) {
	server_txt <- capture.output(server_function)
	ui_txt <- capture.output(ui_function)
	# Strip bytecode and environment info
	server_txt <- server_txt[1:(length(server_txt)-2)]
	ui_txt <- ui_txt[3:(length(ui_txt)-3)]
	# Fix the function assignment
	server_txt[1] <- 'shinyServer(function(input, output, session)'
	server_txt[length(server_txt)] <- '})'
	global_txt <- c("library('shiny')")
	if(!missing(pkgs)) {
		global_txt <- c(global_txt, paste0("library('", pkgs, "')"))
	}
	# Save the shiny app files
	cat(server_txt, sep = '\n', file = 'shiny/server.R')
	cat(ui_txt, sep = '\n', file = 'shiny/ui.R')
	cat(global_txt, sep = '\n', file = 'shiny/global.R')
	# Start the app
	if(run_app) {
		runApp(appDir = 'shiny/')
	}
}

save_shiny_app(ui_function = visualMLE::shiny_ui,
			   server_function = visualMLE::shiny_server,
			   pkgs = c('visualMLE', 'reshape2'))

