dashboardPagePlus(
  header = dashboardHeaderPlus(title = "RNA-seq DE"), title = "RNAseqDE",
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Load dataset and parameters", tabName = "tab_LOAD", icon = icon("database")),
      menuItem("Result", tabName = "tab_RES", icon = tags$i(class = "fas fa-dragon")),
      menuItem("Analysis", tabName = "tab_ANA", icon = icon("analytics"))
    )
  ),
  body = dashboardBody(
    # useShinyalert(),
    # useShinyFeedback(),
    useShinyjs(),
    tabItems(

      # load dataset ------------------------------------------------------------
      tabItem(
        "tab_LOAD",
        fluidRow(
          box(
            width = 2, # box with the file input
            title = "Comptage Table",
            TableInput("comptage_table")
          ),
          box(
            width = 2, # box for the parameters files
            title = "parameters files",
            FileInput("param_in", "Select parameters file", accept = c("application/json"))
          ),

          #
          hidden(boxWithId(
            width = 8,
            id = "box_DATASET", title = "Extract of the dataset",
            rHandsontableOutput("comptage_table-table_DATASET")
          )),
          hidden(boxWithId(
            width = 12,
            id = "box_PARAM", title = "Parameters",

            boxParameters("parameters")
          )),
          column(3, offset = 9, hidden(
            downloadBttn("down_PARAM", label = "Download the parameters", style = "bordered", color = "primary"),
            actionBttn("but_DATASET", "Continue", style = "simple", color = "primary")
          ))
        )
      ),

      # Result of the DE --------------------------------------------------------
      tabItem(
        "tab_RES",
        fluidRow(
          box_PlotOutput_ui("normalization", "Normalizations"),
          comparison_BoxOutput_ui("comparison", "Comparisons"),
          box_PlotOutput_ui("heatmap", "Heatmaps"),
          column(3,
            offset = 9,
            downloadBttn("down_RESULT", style = "bordered", color = "primary"),
            actionBttn("but_RES", "Continue", style = "simple", color = "primary")
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
            box_pca_ui("PCA")
          )),

          # tSNE
          hidden(boxWithId(
            id = "box_tSNE", title = "tSNE", width = 6,
            box_tsne_ui("tSNE")
          )),

          # SOM
          hidden(boxWithId(
            id = "box_SOM", title = "Self Organizing Map", width = 12,
            box_som_ui("self_organizing_map")
          )),

          # DBSCAN
          hidden(boxWithId(
            id = "box_DBSCAN", title = "DBSCAN", width = 6,
            box_dbscan_ui("DBSCAN")
          )),

          # ABOD
          hidden(boxWithId(
            id = "box_ABOD", title = "ABOD", width = 6,
            box_abod_ui("ABOD")
          )),

          # isolation forest
          hidden(boxWithId(
            id = "box_ISOFOR", title = "isolation forest", width = 6,
            box_isofor_ui("isolation_forest")
          )),

          downloadBttn("down_ANA",  style = "bordered", color = "primary")
        )
      )
    )
  )
)
