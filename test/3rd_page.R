devtools::load_all("/data/RNA-seq_project/pipeline/RNASeqDE/")
library(shinyjs)
library(shiny)
library(shinyWidgets)
library(purrr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)

ui <- dashboardPagePlus(
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



server <- function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })


  # Analysis ----------------------------------------------------------------

  # the object of the analysis
  ana_object <- reactiveValues()
  mat_res <- reactiveVal(as.matrix(fread("/data/RNA-seq_project/pipeline/RNASeqDE/test/test.txt"), rownames = "name_gene"))

  observeEvent(input$chkgrp_TOOLS, {

    # apperance of the download button if there is at least one selection in chkgrp tools
    # the . is the element of the vector
    c("down_ANA", "down_ANA_bttn") %>% walk(~ if (!is.null(input$chkgrp_TOOLS)) showElement(.) else hideElement(.))

    # toggle the elements when actif
    # the .x represent the box variable
    # the .y represent the id variable
    tools = as.data.frame(matrix(c(
      "box_PCA", "PCA",
      "box_tSNE", "tSNE",
      "box_SOM", "SOM",
      "box_DBSCAN", "DBSCAN",
      "box_ABOD", "ABOD",
      "box_ISOFOR", "ISOFOR"
    ), ncol = 2, byrow = T), stringsAsFactors = F)

    tools %>% pwalk(~ toggleElement(.x, condition = .y %in% input$chkgrp_TOOLS))

    tools[, 2] %>% discard(~ . %in% input$chkgrp_TOOLS) %>% walk(~ walk2(., names(ana_object[[.]]), function(x, y) ana_object[[x]][[y]] <<- NULL))

  }, ignoreNULL = F)


  update = reactive({
    c(
      "PCA",
      "tSNE",
      "SOM",
      "DBSCAN",
      "ABOD",
      "ISOFOR"
    ) %>% map_lgl(~ . %in% input$chkgrp_TOOLS && is.null(ana_object[[.]][[tolower(.)]]))
  })

  observe(print(is(update())))

  ana_object$PCA <- callModule(PCA_box_server, "PCA", mat_res, reactive(update()[1]))
  ana_object$tSNE = callModule(tSNE_box_server, "tSNE", mat_res, reactive(update()[2]))
  ana_object$SOM = callModule(SOM_box_server, "self_organizing_map", mat_res, reactive(update()[3]))
  ana_object$DBSCAN = callModule(DBSCAN_box_server, "DBSCAN", mat_res, reactive(update()[4]))
  ana_object$ABOD = callModule(ABOD_box_server, "ABOD", mat_res, reactive(update()[5]))
  ana_object$ISOFOR = callModule(ISOFOR_box_server, "isolation_forest", mat_res, reactive(update()[6]))


  output$down_ANA <- downloadHandler(
    filename = function() paste0("analysis_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {

      browser()
      showNotification("In prepartion for the download")

      list_ana <- reactiveValuesToList(ana_object)
      path <- file.path(tempdir(), "analysis")
      dir.create(path, showWarnings = F, recursive = T)
      data <- as.data.table(mat_res(), keep.rownames = T)

      nb_col_before <- ncol(data)
      with(list_ana, {
        suppressWarnings(data[, ":="(som_nb_neuron_cluster = som$pred,
                                     tsne_cluster = tsne$dbscan$cluster,
                                     dbscan_cluster = dbscan$cluster,
                                     outliers_pca = pca$result,
                                     outliers_tsne = if (is.null(tsne)) NULL else tsne$scan$cluster == 0,
                                     outliers_som = if (is.null(som)) NULL else with(som, pred %in% as.numeric(data[dist > quantile(dist, 0.95), rn])),
                                     outliers_dbscan = if (is.null(dbscan)) NULL else dbscan$cluster == 0,
                                     outliers_abod = if (is.null(abod)) NULL else with(abod, abod < quantile(abod, 0.05)),
                                     outliers_isolation_forest = if (is.null(isofor)) NULL else with(isofor, isofor > quantile(isofor, 0.95))
        )])
      })




      data[, outliers_method := rowSums(.SD) / ncol(.SD), .SDcols = replace(grepl("^outliers", names(data)), 1:nb_col_before, F)]

      fwrite(data[outliers_method != 0, .SD, .SDcols = replace(grepl("^outliers", names(data)), 1:nb_col_before, T)], file.path(path, "outliers.csv"), sep = "\t")
      fwrite(data[, .SD, .SDcols = replace(grepl("cluster$", names(data)), 1:nb_col_before, T)], file.path(path, "cluster.csv"), sep = "\t")

      pca_save(list_ana$pca$pca, input$num_PCA, path)
      plot_list_save(list_ana$tsne, path)
      plot_list_save(list_ana$som, path)

      zip(file, path, flags = "-jr9Xm")
    }
  )
}


shinyApp(ui, server, options = list(
  "shiny.launch.browser" = T,
  "shiny.trace" = T, "shiny.autoreload" = T,
  "shiny.reactlog" = T, "shiny.fullstacktrace" = T
))
