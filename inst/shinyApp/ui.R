dashboardPage(
  dashboardHeader(title = "RNA-seq DE application"),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Load dataset and parameters", tabName = "tab_LOAD"),
      menuItem("Result", tabName = "tab_RES")
    )
  ),
  dashboardBody(
    useShinyalert(),
    useShinyFeedback(),
    useShinyjs(),
    tabItems(

      # load dataset ------------------------------------------------------------
      tabItem(
        "tab_LOAD",
        fluidRow(
          box(
            width = 2, # box with the file input
            title = "Comptage Table",
            fileInput("file_DATASET", "Select data file", accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )),
            numericInput("num_skip_line", "Skip lines", value = 0, min = 0),
            radioButtons("rad_decimal", "Decimal:", choices = c(
              Dot = ".",
              Comma = ","
            ))
          ),
          box(
            width = 2, # box for the parameters files
            title = "parameters files",
            fileInput("file_PARAM", "Select parameters file", accept = c("application/json"))
          ),

          #
          hidden(boxWithId(
            width = 8,
            id = "box_DATASET", title = "Extract of the dataset",
            dataTableOutput("table_DATASET")
          )),
          hidden(boxWithId(
            width = 12,
            id = "box_PARAM", title = "Parameters",

            h3("Names columns"), textOutput("txt_COLNAMES"), hr(),
            # table Group
            h3("Groups"),
            dataTableOutput("table_GRP"),
            tableOutput("txt_GRP"),
            hr(),
            # table condtitions
            h3("Conditions"),
            dataTableOutput("table_COND"),
            tableOutput("txt_COND"),
            hr(),
            # table contrast
            h3("contrast"),
            actionButton("but_ADD_ROW", "Add row"),
            actionButton("but_DEL_ROW", "Delete seleted row(s)"),
            dataTableOutput("table_CONTRAST")
            #           tags$script("$(document).on('click', '#table_SUMMARY_column button', function () {
            #                 Shiny.onInputChange('lastClickId',this.id);
            #                 Shiny.onInputChange('lastClick', Math.random())
            # });")
          )),
          column(1, offset = 11, hidden(
            actionButton("but_DATASET", "Continue"),
            tags$style("#but_DATASET {
                          background-color: #0080ff;
                          color: white;
                       }")
          ))
        )
      )

      # ### Result of the DE
      # tabItem("tab_RES",
      #         fluidRow()
      #         )
    )
  )
)
