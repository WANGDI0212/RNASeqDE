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


  # the matrix of the result
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








  # the object of the analysis
  ana_object <- reactiveValues()

  observeEvent(input$chkgrp_TOOLS, {

    # apperance of the download button if there is at least one selection in chkgrp tools
    # the . is the element of the vector
    c("down_ANA", "down_ANA_bttn") %>% walk(~ toggleElement(., condition = !is.null(input$chkgrp_TOOLS)))

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

  }, ignoreNULL = F, ignoreInit = T)

  ana_object = callModule(PCA_box_server, "PCA", ana_object, mat_res, reactive(is.null(ana_object$pca) && "PCA" %in% input$chkgrp_TOOLS))

  # tSNE box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_SNE_EPSILON
    input$num_SNE_MIN
  }, {
    if ("tSNE" %in% input$chkgrp_TOOLS) {
      ana_object$tsne <- tsne_analysis(data = mat_res(), tsne = ana_object$tsne$tsne, epsilon = input$num_tSNE_EPSILON, minpts = input$num_tSNE_MIN)

      output$plot_tSNE <- renderPlot(ana_object$tsne$tsne_plot)
      output$txt_tSNE <- renderText(print_dbscan(ana_object$tsne$dbscan))
    } else {
      ana_object$tsne <- NULL
    }
  }, ignoreNULL = F, ignoreInit = T)



  # DBSCAN box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_DBSCAN_EPSILON
    input$num_DBSCAN_MIN
  }, {
    if ("DBSCAN" %in% input$chkgrp_TOOLS) {
      ana_object$dbscan <- dbscan_analysis(mat_res(), ana_object$dbscan, epsilon = input$num_DBSCAN_EPSILON, minpts = input$num_DBSCAN_MIN)
      output$txt_DBSCAN <- renderText(print_dbscan(ana_object$dbscan))
    } else {
      ana_object$dbscan <- NULL
    }
  }, ignoreNULL = F, ignoreInit = T)


  # ABOD box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_ABOD_KNN
    input$num_ABOD_QUANTILE
  }, {
    if ("ABOD" %in% input$chkgrp_TOOLS) {
      ana_object$abod <- abod_analysis(mat_res(), ana_object$abod, k = input$num_ABOD_KNN)
      output$txt_ABOD <- renderPrint(with(ana_object$abod, {
        cat("The summary of the analysis : ",
          paste(capture.output(summary(abod)), collapse = "\n"),
          outliers_number(sum(abod < quantile(abod, input$num_ABOD_QUANTILE))),
          sep = "\n\n"
        )
      }))
    } else {
      ana_object$abod <- NULL
    }
  }, ignoreNULL = F, ignoreInit = T)



  # ISOFOR box
  observeEvent({
    input$chkgrp_TOOLS
    input$sel_ISOFOR_depth
    input$sel_ISOFOR_ntree
    input$sli_ISOFOR_threshold
  }, {
    if ("ISOFOR" %in% input$chkgrp_TOOLS) {
      ana_object$isofor <- isofor_analysis(mat_res(), ana_object$isofor,
        nTrees = as.integer(input$sel_ISOFOR_ntree),
        phi = 2^as.integer(input$sel_ISOFOR_depth)
      )

      output$txt_ISOFOR <- renderPrint(with(ana_object$isofor, {
        cat("The summary of the analysis : ",
          paste(capture.output(summary(isofor)), collapse = "\n"),
          outliers_number(sum(isofor > quantile(isofor, input$num_ISOFOR_threshold))),
          sep = "\n\n"
        )
      }))
    } else {
      ana_object$isofor <- NULL
    }
  }, ignoreNULL = F, ignoreInit = T)


  # SOM box
  observeEvent({
    input$chkgrp_TOOLS
  }, {
    if ("SOM" %in% input$chkgrp_TOOLS) {
      update <- is.null(ana_object$som)

      if (update) {
        ana_object$som <- som_analysis(mat_res(), som = ana_object$som$som)

        output$plot_SOM <- renderPlot(
          with(ana_object$som, plot_grid(plot_grid(count, dist), codes, nrow = 2, rel_heights = c(1, 2)))
        )
      }

      # render the text of outliers
      output$txt_SOM <- renderText(outliers_number(ana_object$som$data[dist > quantile(dist, input$num_SOM_QUANTILE), sum(N, na.rm = T)]))
    } else {
      ana_object$som <- NULL
    }
  }, ignoreNULL = F, ignoreInit = T)





  # download the analysis
  output$down_ANA <- downloadHandler(
    filename = function() paste0("analysis_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {
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



      # the score of the outliers methods
      data[, outliers_method := rowSums(.SD) / ncol(.SD), .SDcols = replace(grepl("^outliers", names(data)), 1:nb_col_before, F)]

      fwrite(data[outliers_method != 0, .SD, .SDcols = replace(grepl("^outliers", names(data)), 1:nb_col_before, T)], file.path(path, "outliers.csv"), sep = "\t")
      fwrite(data[, .SD, .SDcols = replace(grepl("cluster$", names(data)), 1:nb_col_before, T)], file.path(path, "cluster.csv"), sep = "\t")

      # save all the plot in the file
      pca_save(list_ana$pca$pca, input$num_PCA, path)
      plot_list_save(list_ana$tsne, path)
      plot_list_save(list_ana$som, path)

      zip(file, path, flags = "-jr9Xm")
    }
  )
}
