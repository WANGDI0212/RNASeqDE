dashboardPage(
  dashboardHeader(title = "RNA-seq DE application"),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Load dataset and parameters", tabName = "tab_LOAD"),
      menuItem("Result", tabName = "tab_RES"),
      menuItem("Analysis", tabName = "tab_ANA")
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
            rHandsontableOutput("table_DATASET")
          )),
          hidden(boxWithId(
            width = 12,
            id = "box_PARAM", title = "Parameters",

            h3("Names columns"), textOutput("txt_COLNAMES"), hr(),
            # table Group
            h3("Groups"),
            rHandsontableOutput("table_GRP"),
            tableOutput("txt_GRP"),
            hr(),
            # table condtitions
            h3("Conditions"),
            rHandsontableOutput("table_COND"),
            tableOutput("txt_COND"),
            hr(),
            # table contrast
            h3("contrast"),
            rHandsontableOutput("table_CONTRAST")
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
      ),

      # Result of the DE --------------------------------------------------------
      tabItem(
        "tab_RES",
        fluidRow(
          box(
            width = 12, title = "Normalizations",
            plotOutput("plot_NORM")
          ),
          box(
            width = 12, title = "Comparisons",
            column(6, selectInput("sel_COMP", "select the comparison you want", choices = NULL)),
            column(6, numericInput("num_Pvalue", "Pvalue adjusted", min = 0, max = 1, value = 1)),
            column(12, plotOutput("plot_COMP")),
            column(12, tableOutput("tab_COMP"))
          ),
          box(
            title = "heatmaps", width = 12,
            column(6, plotOutput("plot_HEATMAP_NON_CONTRAST")),
            column(6, plotOutput("plot_HEATMAP_CONTRAST"))
          ),
          column(1,
            offset = 11,
            actionButton("but_RES", "Continue"),
            tags$style("#but_RES {
                          background-color: #0080ff;
                          color: white;
                       }")
          )
        )
      ),


      # Analysis of the result --------------------------------------------------

      tabItem(
        "tab_ANA",
        fluidRow(
          box(
            width = 12, title = "Tools for analysis",
            checkboxGroupInput("chkgrp_TOOLS", "Choose the tools",
              inline = T,
              choices = c("PCA", "tSNE", "self organizing map" = "som", "DBSCAN", "ABOD", "isolation forest" = "isofor")
            )
          ),

          hidden(boxWithId(
            id = "box_PCA", title = "PCA", width = 6,
            textOutput("txt_PCA"),
            plotOutput("plot_PCA")
          ))
        )
      )
    )
  )
)
