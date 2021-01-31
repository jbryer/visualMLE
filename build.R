library(devtools)

#Package building
document()
install()
build()
build_vignettes()
check(cran=TRUE)
release()

visualMLE::shiny_mle()
vignette('mle', package = 'visualMLE')
