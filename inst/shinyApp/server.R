function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_RES")
    hideMenuItem("tab_ANA")
  })


  # load dataset ------------------------------------------------------------



  # import the dataset we will use for the rest
  inv <- callModule(csvFile, "comptage_table")
  param <- callModule(AfterDataset, "comptage_table", inv)

  observeEvent(input$"comptage_table-file", {
    # show the box
    c(
      "box_DATASET",
      "box_PARAM",
      "but_DATASET",
      "down_PARAM",
      "down_PARAM_bttn"
    ) %>% walk(showElement)
  })


  param <- callModule(parametersInput_server, "param_in", reactive(colnames(inv())), param)
  param <- callModule(parameterBox_server, "parameters", reactive(colnames(inv())), param)



  output$down_PARAM <- downloadHandler(
    filename = function() {
      paste0("Parameters", Sys.Date(), ".json")
    },
    content = function(file) {
      write_parameter_file(reactiveValuesToList(param), file)
    },
    contentType = "application/json"
  )


  # Result ------------------------------------------------------------------

  plot_list <- reactiveValues()
  data_comp <- reactiveVal()

  observeEvent(input$but_DATASET, {

    # Before the calculation
    count <- copy(inv())
    group <- as.vector(param$groups)
    conditions <- as.vector(param$conditions)
    contrast <- param$contrast

    plot_normalization <- 0
    plot_comp <- 0

    withProgress(message = "Calculation in Progress", {
      incProgress(0, detail = "filter the data")
      count <- filter(count)

      incProgress(1 / 4, detail = "normalize the data")
      tmp <- normalization(count, group)
      y <- tmp$normalized_dataset
      plot_normalization <- tmp$plot

      incProgress(1 / 4, detail = "Differential expression")
      fit <- DE(y, conditions)

      incProgress(1 / 4, detail = "Do the comparison")
      data_comp(comparison(fit, contrast))

      setProgress(1, detail = "End of the calculation")
    })


    showMenuItem("tab_RES")
    updateTabItems(session, "mnu_MENU", "tab_RES")

    plot_list$normalization_raw <- plot_normalization$raw
    plot_list$normalization <- plot_normalization$normalize

    # The plot on the normalisation
    callModule(twoPlot_server, "normalization", reactive(plot_list$normalization_raw), reactive(plot_list$normalization))

    # update the input from the data we have
    updateSelectInput(session, "comparison-comparison", choices = data_comp()[, unique(comp_name)], selected = data_comp()[, unique(comp_name)][1])
    updateSliderInput(session, "comparison-pvalue", value = 0.05, min = 0, max = round(data_comp()[, max(pval_adj)]), step = 0.001)
  })


  plot_list <- callModule(comparison_box_server, "comparison", data_comp, plot_list)
  callModule(twoPlot_server, "comparison", reactive(plot_list$Smear_plot), reactive(plot_list$volcano_plot))
  callModule(twoPlot_server, "heatmap", reactive(plot_list$heatmap_non_contrast), reactive(plot_list$heatmap_contrast))




  output$down_RESULT <- downloadHandler(
    filename = function() paste0("result_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {
      showNotification("In prepartion for the download")
      path <- file.path(tempdir(), "result")
      dir.create(path, showWarnings = F, recursive = T)
      # write the file with all the data
      fwrite(data_comp(), file.path(path, "comparison.csv"), sep = "\t")

      # take all the plot
      list_plot <- reactiveValuesToList(plot_list)

      # remove the plot we will create non the less
      list_plot$Smear_plot <- NULL
      list_plot$volcano_plot <- NULL

      # save the other plot
      plot_list_save(list_plot, path)

      # save all the plot for each comparison
      sapply(split(data_comp(), by = "comp_name"), function(subdata) {
        title <- subdata[, unique(comp_name)]

        is_inferior <- subdata[, as.character(pval_adj < input$"comparison-pvalue")]

        name_legend <- paste("adj PValue <=", input$"comparison-pvalue")
        gg_begin <- ggplot(subdata, aes(col = is_inferior, shape = is_inferior)) +
          labs(shape = name_legend, colour = name_legend, title = title) +
          scale_color_manual(values = color_true_false) +
          scale_shape_manual(values = shape_true_false) + theme_gray()

        ggsave(paste0(title, "_Smear.svg"), gg_begin + geom_point(aes(x = AveLogCPM, y = logFC)) + geom_smooth(aes(x = AveLogCPM, y = logFC)), path = path)
        ggsave(paste0(title, "_volcano.svg"), gg_begin + geom_point(aes(x = logFC, y = -log10(pval_adj))) + ylab("-log10(adj PValue)"), path = path)
      }, simplify = F)

      zip(file, path, flags = "-jr9Xm")
    }
  )




  # Analysis ----------------------------------------------------------------


  # Analysis ----------------------------------------------------------------

  # the object of the analysis
  ana_object <- reactiveValues()
  mat_res <- reactiveVal()

  observeEvent({
    input$but_RES
    if (input$but_RES >= 1) {
      input$"comparison-pvalue"
    }
  }, {
    showMenuItem("tab_ANA")

    mat_res(as.matrix(dcast(data_comp()[pval_adj <= input$"comparison-pvalue"], rn ~ comp_name, value.var = "logFC", fill = 0), rownames = "rn"))

    updateTabItems(session, "mnu_MENU", "tab_ANA")
  }, ignoreInit = T)

  observeEvent(input$chkgrp_TOOLS, {

    # apperance of the download button if there is at least one selection in chkgrp tools
    # the . is the element of the vector
    c("down_ANA", "down_ANA_bttn") %>% walk(~ toggleElement(., condition = !is.null(input$chkgrp_TOOLS)) )

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
