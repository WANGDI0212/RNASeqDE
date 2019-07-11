box_PlotOutput_ui <- function(id, title = NULL) {
  ns <- NS(id)

  if (is.null(title))
    box(
      width = 12, title = title,
      column(12, plotOutput(ns("plot")))
    )
  else
    column(12, plotOutput(ns("plot")))
}

comparison_BoxOutput_ui <- function(id, title) {
  ns <- NS(id)

  box(
    width = 12, title = title,
    column(6, selectInput(ns("comparison"), "select the comparison you want", choices = NULL)),
    column(6, numericInput(ns("pvalue"), "Pvalue adjusted", min = 0, max = 1, value = 1)),
    column(12, plotOutput(ns("plot"))),
    column(12, tableOutput(ns("table")))
  )
}
