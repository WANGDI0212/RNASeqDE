TableInput <- function(id) {
  ns <- NS(id)

  tagList(
    FileInput(ns("table"), label = "Select data file", accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )),
    numericInput(ns("num_skip_line"), "Skip lines", value = 0, min = 0),
    radioButtons(ns("rad_decimal"), "Decimal:", choices = c(
      Dot = ".",
      Comma = ","
    ))
  )
}


FileInput <- function(id, label, accept) {
  ns <- NS(id)

  fileInput(ns("file"), label, accept = accept)
}


boxParameters = function(id){

  ns = NS(id)

  tagList(
    # Names of the columns
    h3("Names columns"),
    textOutput(ns("colnames")),
    hr(),

    # table Group
    h3("Groups"),
    rHandsontableOutput(ns("group")),
    tableOutput("txt_GRP"),
    hr(),

    # table condtitions
    h3("Conditions"),
    rHandsontableOutput(ns("condition")),
    tableOutput("txt_COND"),
    hr(),

    # table contrast
    h3("contrast"),
    rHandsontableOutput(ns("contrast"))

  )
}













