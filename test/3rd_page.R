devtools::load_all("/data/RNA-seq_project/pipeline/RNASeqDE/")
library(shinyjs)
library(shiny)
library(shinyWidgets)
library(purrr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(configr)

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
            column(12, align = "left", style = "display: flex;flex-direction: row;align-items: center;",
            div(style = "width: 90%", checkboxGroupInput("chkgrp_TOOLS", "Choose the tools",
              inline = T,
              choices = c("PCA", "tSNE", "self organizing map" = "SOM", "DBSCAN", "ABOD", "isolation forest" = "ISOFOR")
            )),
            div(style = "width: 10%", downloadBttn("down_ANA", style = "bordered", color = "primary"), offset = 10))
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
          ))
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
    c("down_ANA", "down_ANA_bttn") %>% walk(~ toggleElement(., condition = !is.null(input$chkgrp_TOOLS)))

    # toggle the elements when actif
    # the .x represent the box variable
    # the .y represent the id variable
    tools <- as.data.frame(matrix(c(
      "box_PCA", "PCA",
      "box_tSNE", "tSNE",
      "box_SOM", "SOM",
      "box_DBSCAN", "DBSCAN",
      "box_ABOD", "ABOD",
      "box_ISOFOR", "ISOFOR"
    ), ncol = 2, byrow = T), stringsAsFactors = F)

    tools %>% pwalk(~ toggleElement(.x, condition = .y %in% input$chkgrp_TOOLS))

    tools[, 2] %>%
      discard(~ . %in% input$chkgrp_TOOLS) %>%
      walk(~ walk2(., names(ana_object[[.]]), function(x, y) ana_object[[x]][[y]] <<- NULL))
  }, ignoreNULL = F)


  update <- reactive({
    c(
      "PCA",
      "tSNE",
      "SOM",
      "DBSCAN",
      "ABOD",
      "ISOFOR"
    ) %>% map_lgl(~ . %in% input$chkgrp_TOOLS && is.null(ana_object[[.]][[tolower(.)]]))
  })

  ana_object$PCA <- callModule(PCA_box_server, "PCA", mat_res, reactive(update()[1]))
  ana_object$tSNE <- callModule(tSNE_box_server, "tSNE", mat_res, reactive(update()[2]))
  ana_object$SOM <- callModule(SOM_box_server, "self_organizing_map", mat_res, reactive(update()[3]))
  ana_object$DBSCAN <- callModule(DBSCAN_box_server, "DBSCAN", mat_res, reactive(update()[4]))
  ana_object$ABOD <- callModule(ABOD_box_server, "ABOD", mat_res, reactive(update()[5]))
  ana_object$ISOFOR <- callModule(ISOFOR_box_server, "isolation_forest", mat_res, reactive(update()[6]))


  output$down_ANA <- downloadHandler(
    filename = function() paste0("analysis_", gsub(" ", "_", Sys.time()), ".zip"),
    contentType = "application/zip",
    content = function(file) {
      showNotification("In prepartion for the download")

      # initialization + path where to write
      list_ana <- reactiveValuesToList(ana_object) %>% map(~ reactiveValuesToList(.))
      path <- file.path(tempdir(), "analysis")
      dir.create(path, showWarnings = F, recursive = T)
      data <- as.data.table(mat_res(), keep.rownames = T)

      # take the patterns to keep all values we want
      patterns = paste0("(", input$chkgrp_TOOLS, ")", collapse = "|")
      patterns = sub("SOM", "self_organizing_map", patterns)
      patterns = sub("ISOFOR", "isolation_forest", patterns)

      # convert the input list and search for what we really want
      inputlist = reactiveValuesToList(input)
      inputlist = inputlist[names(inputlist) %>% keep(grepl, pattern = patterns) %>% discard(grepl, pattern = "selectized")]
      names(inputlist) = gsub("quantile-quantile", "quantile", names(inputlist))

      # put the configuration in a file
      name = names(inputlist) %>% strsplit("-") %>% map_chr(1) %>% unique
      name %>% map(~ {
        inputlist[grepl(., names(inputlist))] %>% set_names(~ strsplit(., "-") %>% map_chr(2))
      }) %>% set_names(name) %>% write.config(file.path = file.path(path, 'parameters.txt'))


      # the data from the analysis
      nb_col_before <- ncol(data)
      with(list_ana, {
        suppressWarnings(data[, ":="(
          som_nb_neuron_cluster = SOM$som$pred,
          tsne_cluster = tSNE$dbscan$cluster,
          dbscan_cluster = DBSCAN$dbscan$cluster,
          outliers_pca = PCA$res$result,
          outliers_tsne = if (is.null(tSNE$tsne)) NULL else tSNE$dbscan$cluster == 0,
          outliers_som = SOM$som$res,
          outliers_dbscan = if (is.null(DBSCAN$dbscan)) NULL else DBSCAN$dbscan$cluster == 0,
          outliers_abod = ABOD$res,
          outliers_isolation_forest = ISOFOR$res
        )])
      })



      # the score of the outliers methods
      data[, outliers_method := rowSums(.SD) / ncol(.SD), .SDcols = replace(grepl("^outliers", names(data)), 1:nb_col_before, F)]

      # replace the space by _ in all the column names (for excel)
      setnames(data, names(data), gsub(" ", "_", names(data)))

      # write files
      fwrite(data[outliers_method != 0, .SD, .SDcols = replace(grepl("^outliers", names(data)), 1:nb_col_before, T)], file.path(path, "outliers.csv"), sep = "\t")
      fwrite(data[, .SD, .SDcols = replace(grepl("cluster$", names(data)), 1:nb_col_before, T)], file.path(path, "cluster.csv"), sep = "\t")

      # save all the plot
      pca_save(list_ana$PCA$pca, input$`PCA-sphere_radius`, path)
      plot_list_save(list_ana$tSNE, path)
      plot_list_save(list_ana$SOM$som, path)

      # zip the file and
      zip(file, path, flags = "-jr9Xm")
    }
  )
}


shinyApp(ui, server, options = list(
  "shiny.launch.browser" = T,
  "shiny.trace" = T, "shiny.autoreload" = T,
  "shiny.reactlog" = T, "shiny.fullstacktrace" = T
))
