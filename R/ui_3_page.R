box_pca_ui <- function(id) {
  ns <- NS(id)

  tagList(
    verbatimTextOutput(ns("inertia")),
    plotOutput(ns("plot")),

    # To put the two boxes (numeric input and text output) in the same line, and the text centered
    column(12,
      align = "left", style = "display: flex;flex-direction: row;align-items: center;",
      div(style = "width: 50%", numericInput(ns("sphere_radius"), "sphere radius", value = 2, min = 0, step = 0.1)),
      div(style = "width: 49%; text-align: center", textOutput(ns("outliers")))
    )
  )
}

box_dbscan_ui <- function(id) {
  ns <- NS(id)

  tagList(
    column(3, numericInput(ns("epsillon"), "Epsillon", 0.5, min = 0, step = 0.1)),
    column(9, sliderInput(ns("minPts"), "MinPts", min = 1, max = 20, step = 1, value = 5)),
    verbatimTextOutput(ns("result"))
  )
}

box_tsne_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("DBSCAN"),
    box_dbscan_ui(ns("dbscan")),
    br(),
    column(12, plotOutput(ns("plot")))
  )
}

Anomaly_Score_Threshold_ui <- function(id, value = 0.95) {
  ns <- NS(id)

  sliderInput(ns("quantile"), "Anomaly Score Threshold", value = value, min = 0, max = 1, step = 0.01)
}

box_som_ui <- function(id) {
  ns <- NS(id)

  tagList(
    Anomaly_Score_Threshold_ui(ns("quantile")),
    textOutput(ns("outliers")),
    column(12, plotOutput(ns("plot"), height = "1000px"))
  )
}


box_abod_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sliderInput(ns("knn"), "Number of nearest neighbours", 20, min = 1, max = 40, step = 1),
    Anomaly_Score_Threshold_ui(ns("quantile"), value = 0.05),
    verbatimTextOutput(ns("outliers"))
  )
}


#' @importFrom purrr map
box_isofor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    Anomaly_Score_Threshold_ui(ns("quantile")),
    column(6, selectInput(ns("tree_depth"), "Tree Depth", choices = c(3, 4, 5, 6, 7, 8), selected = 5)),
    column(6, selectInput(ns("trees_number"), "Number of Trees", choices = unlist(map(c(100, 1000), ~ . * c(1, 2, 5))), selected = 100)),
    verbatimTextOutput(ns("outliers"))
  )
}
