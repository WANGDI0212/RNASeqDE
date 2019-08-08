devtools::load_all(path = '/data/RNA-seq_project/pipeline/RNASeqDE/')
library(reactlog)
library(rhandsontable)
library(cowplot)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(shinyFeedback)
library(shiny)
library(shinyWidgets)
library(purrr)
library(ggplot2)
library(configr)


shinyOptions("shiny.launch.browser" = T)
shinyOptions("shiny.trace" = T)
shinyOptions("shiny.autoreload" = T)
options(shiny.reactlog=TRUE)
options(shiny.fullstacktrace = TRUE)
options(shiny.error = NULL)

# set maximum input file size (here 30Mo)
options(shiny.maxRequestSize = 30 * 1024^2)





