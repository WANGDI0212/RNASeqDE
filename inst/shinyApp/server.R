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
  inv = callModule(csvFile, "comptage_table")
  param = callModule(AfterDataset, "comptage_table", inv)

  observeEvent(input$"comptage_table-file", {
    # show the box
    showElement("box_DATASET")
    showElement("box_PARAM")
    showElement("but_DATASET")
    showElement("down_PARAM")

  })


  observe(print(names(inv())))


  param = callModule(parametersInput_serveur, "param_in", reactive(colnames(inv())), param)
  param = callModule(parameterBox_server, "parameters", reactive(colnames(inv())), param)



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

  data_comp <- 0
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
      data_comp <<- comparison(fit, contrast)

      setProgress(1, detail = "End of the calculation")
    })


    showMenuItem("tab_RES")
    updateTabItems(session, "mnu_MENU", "tab_RES")

    plot_list$normalization_raw <- plot_normalization$raw
    plot_list$normalization <- plot_normalization$normalize

    # The plot on the normalisation
    output$plot_NORM <- renderPlot(plot_grid(plot_list$normalization_raw, plot_list$normalization, nrow = 1, align = "v"))

    # update the input from the data we have
    updateSelectInput(session, "sel_COMP", choices = data_comp[, unique(comp_name)], selected = data_comp[, unique(comp_name)][1])
    updateSliderInput(session, "num_Pvalue", value = 0.05, min = 0, max = round(data_comp[, max(pval_adj)]), step = 0.001)
  })



  observeEvent(input$num_Pvalue, {
    # update the table with the Pvalue you want
    output$tab_COMP <- renderTable({
      data_comp[pval_adj <= input$num_Pvalue, .(total = .N, up = sum(logFC > 0), down = sum(logFC < 0)), by = comp_name ]
    })


    # update the heatmap
    # keep the rows that have at least one of the Pvalue is down to the Pvalue they want
    keep.row <- data_comp[, sum(pval_adj <= input$num_Pvalue) >= 1, by = rn][(V1)]
    if (nrow(keep.row) >= 1) {
      plot_list$heatmap_non_contrast <- heatmap_ggplot(data_comp[
        keep.row,
        on = "rn"
      ], "heatmap non contrasted")

      plot_list$heatmap_contrast <- heatmap_ggplot(
        data_comp[pval_adj <= input$num_Pvalue],
        "heatmap contrasted"
      )
    }
    output$plot_HEATMAP <- renderPlot(plot_grid(plot_list$heatmap_non_contrast, plot_list$heatmap_contrast, nrow = 1, align = "v"))
  }, ignoreInit = T)




  # update the graph of the comparison
  observeEvent({
    input$sel_COMP
    input$num_Pvalue
  }, {

    # to avoid rewrite the same lines over and over
    name_legend <- paste("adj PValue <=", input$num_Pvalue)
    gg_begin <- ggplot(data_comp[comp_name == input$sel_COMP], aes(
      col = as.character(pval_adj < input$num_Pvalue),
      shape = as.character(pval_adj < input$num_Pvalue)
    )) +
      ggtitle(input$sel_COMP) +
      scale_color_manual(name = "Outliers", values = color_true_false) +
      scale_shape_manual(name = "Outliers", values = shape_true_false) + theme_gray()


    output$plot_COMP <- suppressMessages(renderPlot({
      a <- gg_begin + geom_point(aes(x = AveLogCPM, y = logFC)) + geom_smooth(aes(x = AveLogCPM, y = logFC))
      b <- gg_begin + geom_point(aes(x = logFC, y = -log10(pval_adj))) + ylab("-log10(adj PValue)")

      plot_grid(a, b, nrow = 1, align = "v")
    }))
  }, ignoreInit = T)



  output$down_RESULT <- downloadHandler(
    filename = function() paste0("result_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {
      showNotification("In prepartion for the download")
      path <- file.path(tempdir(), "result")
      plot_list_save(reactiveValuesToList(plot_list), path)
      fwrite(data_comp, file.path(path, "comparison.csv"), sep = "\t")

      sapply(split(data_comp, by = "comp_name"), function(subdata) {
        title <- subdata[, unique(comp_name)]

        gg_begin <- ggplot(subdata, aes(col = pval_adj < input$num_Pvalue, shape = pval_adj < input$num_Pvalue)) +
          ggtitle(title) +
          scale_color_manual(name = paste("adj PValue <", input$num_Pvalue), values = c("blue", "red")) +
          scale_shape_manual(name = paste("adj PValue <", input$num_Pvalue), values = c(16, 3))

        ggsave(paste0(title, ".png"), gg_begin + geom_point(aes(x = AveLogCPM, y = logFC)), path = path)
        ggsave(paste0(title, "_volvcano.png"), gg_begin + geom_point(aes(x = logFC, y = -log10(pval_adj))) + ylab("-log10(adj PValue)"), path = path)
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
      input$num_Pvalue
    }
  }, {
    showMenuItem("tab_ANA")

    mat_res(as.matrix(dcast(data_comp[pval_adj <= input$num_Pvalue], rn ~ comp_name, value.var = "logFC", fill = 0), rownames = "rn"))

    updateTabItems(session, "mnu_MENU", "tab_ANA")
  }, ignoreInit = T)








  # the object of the analysis
  ana_object <- reactiveValues()

  observeEvent(input$chkgrp_TOOLS, toggleElement("down_ANA", condition = !is.null(input$chkgrp_TOOLS)), ignoreNULL = F)

  # PCA box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_PCA
  }, {
    toggleElement("box_PCA", condition = "PCA" %in% input$chkgrp_TOOLS)

    if ("PCA" %in% input$chkgrp_TOOLS) {
      ana_object$pca <- pca_analysis(mat_res(), pca = ana_object$pca$pca, radius = input$num_PCA_sphere_radius)

      # the plot
      output$plot_PCA <- renderPlot(plot_grid(ana_object$pca$corcircle, ana_object$pca$axis, nrow = 1))
      output$txt_PCA_out <- renderText(outliers_number(sum(ana_object$pca$result)))
    } else {
      ana_object$pca <- NULL
    }
  }, ignoreNULL = F, ignoreInit = T)



  # tSNE box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_SNE_EPSILON
    input$num_SNE_MIN
  }, {
    toggleElement("box_tSNE", condition = "tSNE" %in% input$chkgrp_TOOLS)

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
    toggleElement("box_DBSCAN", condition = "DBSCAN" %in% input$chkgrp_TOOLS)

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
    toggleElement("box_ABOD", condition = "ABOD" %in% input$chkgrp_TOOLS)

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
    toggleElement("box_ISOFOR", condition = "ISOFOR" %in% input$chkgrp_TOOLS)

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
    toggleElement("box_SOM", condition = "SOM" %in% input$chkgrp_TOOLS)

    if ("SOM" %in% input$chkgrp_TOOLS) {
      update <- is.null(ana_object$som)

      if(update){
        ana_object$som <- som_analysis(mat_res(), som = ana_object$som$som)

        output$plot_SOM <- renderPlot(
          with(ana_object$som, plot_grid(plot_grid(count, dist), codes, nrow = 2, rel_heights = c(1, 2)))
        )
      }

      # render the text of outliers
      output$txt_SOM = renderText(outliers_number(ana_object$som$data[dist > quantile(dist, input$num_SOM_QUANTILE), sum(N, na.rm = T)]))

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

      # take the parameters who are in the input for the analysis
      inputlist = reactiveValuesToList(input)
      inputlist = inputlist[-grep("-selectized$", names(inputlist))]
      inputlist = inputlist[grep("(PCA)|(SOM)|(ISOFOR)|(tSNE)|(DBSCAN)|(ABOD)", names(inputlist))]
      strsplit(names(inputlist), "_")
      name = sapply(strsplit(names(inputlist), "_"), function(x) paste(x[-1], collapse = " "))
      write(paste(name, ":", inputlist), file = file.path(path, "input_analysis.txt"))

      # the data from the analysis
      data = as.data.table(mat_res(), keep.rownames = T)
      nb_col_before = ncol(data)
      with(list_ana, {
        suppressWarnings(data[, ':='(som_nb_neuron_cluster = som$pred,
                  tsne_cluster = tsne$dbscan$cluster,
                  dbscan_cluster = dbscan$cluster,
                  outliers_pca = pca$result,
                  outliers_tsne = if (is.null(tsne)) NULL else tsne$scan$cluster == 0,
                  outliers_som = if (is.null(som)) NULL else with(som, pred %in% as.numeric(data[dist > quantile(dist, 0.95), rn])),
                  outliers_dbscan = if (is.null(dbscan)) NULL else dbscan$cluster == 0,
                  outliers_abod = if(is.null(abod)) NULL else with(abod, abod < quantile(abod, 0.05)),
                  outliers_isolation_forest = if(is.null(isofor)) NULL else with(isofor, isofor > quantile(isofor, 0.95))
                  )])
      })



      # the score of the outliers methods
      data[, outliers_method := rowSums(.SD) / ncol(.SD), .SDcols = replace(grepl("^outliers", names(data)), 1:nb_col_before, F)]

      # replace the space by _ in all the column names (for excel)
      setnames(data, names(data), gsub(" ", "_", names(data)))

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
