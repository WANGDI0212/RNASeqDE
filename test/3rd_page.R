devtools::load_all("/data/RNA-seq_project/pipeline/RNASeqDE/")
library(shinyjs)
library(shiny)
library(shinyWidgets)
library(purrr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)

ui = dashboardPagePlus(
  header = dashboardHeaderPlus(title = "RNA-seq DE"), title = "RNAseqDE",
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "mnu_MENU",
      menuItem("Analysis", tabName = "tab_ANA", icon = icon("analytics"))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tabItems(

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

          downloadBttn("down_ANA", style = "bordered", color = "primary")
        )
      )
    )
  )
)



server = function(input, output, session){

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })


  # Analysis ----------------------------------------------------------------

  # the object of the analysis
  ana_object <- reactiveValues()
  mat_res = reactiveVal(as.matrix(fread("test/test.txt"), rownames = "name_gene"))

  observeEvent(input$chkgrp_TOOLS, {

    # apperance of the download button if there is at least one selection in chkgrp tools
    # the . is the element of the vector
    c("down_ANA", "down_ANA_bttn") %>% walk(~ toggleElement(.), condition = !is.null(input$chkgrp_TOOLS))

    # toggle the elements when actif
    # the .x represent the box variable
    # the .y represent the id variable
    as.data.frame(matrix(c(
      "box_PCA", "PCA",
      "box_tSNE", "tSNE",
      "box_SOM", "SOM",
      "box_DBSCAN", "DBSCAN",
      "box_ABOD", "ABOD",
      "box_ISOFOR", "ISOFOR"
    ), ncol = 2, byrow = T)) %>% pwalk(~ toggleElement(.x, condition = .y %in% input$chkgrp_TOOLS))

  }, ignoreNULL = F)

  ana_object = callModule(PCA_box_server, "PCA", ana_object, mat_res, reactive(is.null(ana_object$pca) && "PCA" %in% input$chkgrp_TOOLS))

}


shinyApp(ui, server, options = list("shiny.launch.browser" = T,
                                    "shiny.trace" = T, "shiny.autoreload" = T,
                                    "shiny.reactlog"=T, "shiny.fullstacktrace" = T))
