library(shiny)
library(ggplot2)
library(reshape2)
library(cowplot)
library(tidyverse)
library(DT)
library(visualMLE)

# Can use a different data set if desired
data(mtcars)
thedata <- mtcars

# Keep only numeric and logical columns
numeric.cols <- sapply(mtcars, FUN = function(x) { return(is.numeric(x) | is.logical(x)) })
thedata <- thedata[,numeric.cols]

default.y <- 'mpg' # Default variable selected for the dependent variable
default.x <- 'wt'  # Default variable selected for the independent variable

# NOTE: This app will use row.names to identify and highlight specific points.
#       Make sure the row.names are interpretable.
row_highlights <- c('None', row.names(mtcars))
