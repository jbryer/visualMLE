library(devtools)

#Package building
devtools::document()
devtools::install()
devtools::install(build_vignettes = TRUE)
devtools::build()
devtools::clean_vignettes()
devtools::build_vignettes()
devtools::check(cran=TRUE)
devtools::release()

visualMLE::shiny_mle()
vignette('mle', package = 'visualMLE')
