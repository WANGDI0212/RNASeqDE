dashboardPagePlus(
  dashboardHeaderPlus(title = "RNA-seq DE"),
  dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Load dataset and parameters", tabName = "tab_LOAD", icon = icon("database")),
      menuItem("Result", tabName = "tab_RES", icon = icon("dragon")),
      menuItem("Analysis", tabName = "tab_ANA", icon = icon("analytics"))
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
          )),
          column(3, offset = 9, hidden(
            downloadButton("down_PARAM", "Download the parameters"),
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
            plotOutput("plot_HEATMAP")
          ),
          column(3,
            offset = 9,
            downloadButton("down_RESULT"),
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
              choices = c("PCA", "tSNE", "self organizing map" = "SOM", "DBSCAN", "ABOD", "isolation forest" = "ISOFOR")
            )
          ),

          # PCA
          hidden(boxWithId(
            id = "box_PCA", title = "PCA", width = 6,
            textOutput("txt_PCA"),
            plotOutput("plot_PCA"),
            column(12, align="left", style = "display: flex;flex-direction: row;align-items: center;",
              div(style="width: 50%", numericInput("num_PCA_sphere_radius", "sphere radius", value = 2, min = 0, step = 0.1)),
              div(style = "width: 49%; text-align: center", textOutput("txt_PCA_out"))
            )
          )),

          # tSNE
          hidden(boxWithId(
            id = "box_tSNE", title = "tSNE", width = 6,
            h4("DBSCAN"),
            column(3, numericInput("num_tSNE_EPSILON", "Epsillon", 0.5, min = 0, step = 0.1)),
            column(9, sliderInput("num_tSNE_MIN", "MinPts", min = 1, max = 20, step = 1, value = 5)),
            br(),
            column(12, plotOutput("plot_tSNE")),
            verbatimTextOutput("txt_tSNE")
          )),

          # SOM
          hidden(boxWithId(
            id = "box_SOM", title = "Self Organizing Map", width = 12,
            sliderInput("num_SOM_QUANTILE", "Anomaly Score Threshold", 0.95, min = 0, max = 1, step = 0.01),
            textOutput("txt_SOM"),
            column(12, plotOutput("plot_SOM", height = "1000px"))
          )),

          # DBSCAN
          hidden(boxWithId(
            id = "box_DBSCAN", title = "DBSCAN", width = 6,
            column(3, numericInput("num_DBSCAN_EPSILON", "Epsillon", 0.5, min = 0, step = 0.1)),
            column(9, sliderInput("num_DBSCAN_MIN", "MinPts", min = 1, max = 20, step = 1, value = 5)),
            verbatimTextOutput("txt_DBSCAN")
          )),

          # ABOD
          hidden(boxWithId(
            id = "box_ABOD", title = "ABOD", width = 6,
            column(6, sliderInput("num_ABOD_KNN", "Number of nearest neighbours", 20, min = 1, max = 40, step = 1)),
            column(6, sliderInput("num_ABOD_QUANTILE", "Anomaly Score Threshold", 0.05, min = 0, max = 1, step = 0.01)),
            verbatimTextOutput('txt_ABOD')
          )),

          # isolation forest
          hidden(boxWithId(
            id = "box_ISOFOR", title = "isolation forest", width = 6,
            sliderInput("num_ISOFOR_threshold", "Anomaly Score Threshold", min = 0, max = 1, step = 0.01, value = 0.95),
            column(6, selectInput("sel_ISOFOR_depth", "Tree Depth", choices = c(3,4,5,6,7,8), selected=5)),
            column(6, selectInput("sel_ISOFOR_ntree", "Number of Trees", choices = c(10,20,50,100,200,500), selected = 50)),
            verbatimTextOutput("txt_ISOFOR")
          )),

          downloadButton("down_ANA")
        )
      )
    )
  )
)
