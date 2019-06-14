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

  inv <- reactiveVal()
  param <- reactiveValues()

  # import the dataset we will use for the rest
  observeEvent(ignoreInit = T, {
    input$file_DATASET
    input$num_skip_line
    input$rad_decimal
  }, {
    # file importation
    if (!is.null(input$file_DATASET)) {
      tmp <- suppressWarnings(fread(
        file = input$file_DATASET$datapath,
        skip = ifelse(is.na(input$num_skip_line) || input$num_skip_line == 0, "__auto__", input$num_skip_line),
        data.table = T,
        dec = input$rad_decimal,
        fill = T,
        blank.lines.skip = T
      ))

      inv(tmp)

      # show the box
      showElement("box_DATASET")
      showElement("box_PARAM")
      showElement("but_DATASET")



      # show the content of the table
      output$table_DATASET <- renderRHandsontable({
        rhandsontable(head(tmp, n = 50), stretchH = "all", height = 250, readOnly = T) %>% hot_cols(fixedColumnsLeft = 1)
      })
      output$txt_COLNAMES <- renderText(paste(colnames(tmp)[-1], collapse = ", "))

      param$groups <- matrix(rep("un", ncol(tmp) - 1),
        nrow = 1,
        dimnames = list(NULL, names(tmp)[-1])
      )
      param$conditions <- matrix(rep(paste0("cond", 1:4), each = (ncol(tmp) - 1) / 4, length.out = ncol(tmp) - 1),
        nrow = 1,
        dimnames = list(NULL, names(tmp)[-1])
      )


      # For the contrast table
      condition <- unique(as.vector(param$conditions))
      DT <- diag(nrow = length(condition) - 1, ncol = length(condition))
      DT[row(DT) == col(DT) - 1] <- -1

      colnames(DT) <- condition
      DT <- data.table(
        comparison_names = rev(rev(paste(condition, shift(condition, -1), sep = "_VS_"))[-1]),
        DT
      )

      param$contrast <- DT
    }
  })


  # import the hypothetic parameters files
  observeEvent(input$file_PARAM, {

    # read the json parameters
    tmp <- read_parameter_file(input$file_PARAM$datapath)

    param$groups <- matrix(tmp$groups, nrow = 1, dimnames = list(NULL, names(inv())[-1]))
    param$conditions <- matrix(tmp$conditions, nrow = 1, dimnames = list(NULL, names(inv())[-1]))

    param$contrast <- tmp$contrast
  })

  # take the input paramters in the group table
  observe({
    if (!is.null(input$table_GRP)) {
      param$groups <- hot_to_r(input$table_GRP)
    }
  })

  # take the input paramters in the condition table
  observe({
    if (!is.null(input$table_COND)) {
      param$conditions <- hot_to_r(input$table_COND)
    }
  })

  # if there is a change in the conditions parameters take it to the contrast table
  observeEvent(param$conditions, {
    tmp <- copy(param$contrast) # the copy is important

    # create new column if necessary
    modified <- F
    diff <- setdiff(as.vector(param$conditions)[-1], names(param$contrast))
    if (length(diff) != 0) {
      tmp[, (diff) := 0]
      modified <- T
    }

    # delete some column if nessary
    diff <- setdiff(names(param$contrast)[-1], as.vector(param$conditions))
    if (length(diff) != 0) {
      tmp[, (diff) := NULL]
      modified <- T
    }

    # if the contrast table had been modified reorder the column and update the contrast table
    if (modified) {
      setcolorder(tmp, c("comparison_names", unique(as.vector(param$conditions))))
      param$contrast <- tmp
    }
  })

  # Take the contrast and if there is one suppelementary line take it and replace it by another line filled by 0 and with a random name
  observe({
    if (!is.null(input$table_CONTRAST)) {
      tmp <- hot_to_r(input$table_CONTRAST)

      added_row <- unique(which(is.na(tmp), arr.ind = T)[, "row"])
      if (length(added_row) != 0) {
        set(tmp, added_row, 2:ncol(tmp), 0)
        set(tmp, added_row, 1L, basename(tempfile("comp_")))
      }
      param$contrast <- tmp
    }
  })


  # the tables
  output$table_GRP <- renderRHandsontable({
    rhandsontable(param$groups, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = F, allowColEdit = F)
  })

  output$table_COND <- renderRHandsontable({
    rhandsontable(param$conditions, stretchH = "all") %>%
      hot_context_menu(allowRowEdit = F, allowColEdit = F)
  })

  output$table_CONTRAST <- renderRHandsontable({
    rhandsontable(param$contrast, stretchH = "all") %>%
      hot_validate_numeric(cols = 2:ncol(param$contrast)) %>%
      hot_cols(renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (value < 0) {
               td.style.background = 'lightblue';
               } else if (value > 0) {
               td.style.background = 'Salmon';
               }
               }")
  })

  # to the table functions
  observe({
    output$txt_GRP <- renderTable(table(param$groups))
    output$txt_COND <- renderTable(table(param$conditions))
  })





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
      #   pheatmap(
      #   t(as.matrix(dcast(data_comp, rn ~ comp_name, value.var = "logFC")[keep.row], rownames = "rn")),
      #   scale = "row", main = "heatmap non contrasted", show_colnames = F, silent = T
      # )

      plot_list$heatmap_contrast <- heatmap_ggplot(
        data_comp[pval_adj <= input$num_Pvalue],
        "heatmap contrasted"
      )
    }
    output$plot_HEATMAP <- renderPlot(plot_grid(plot_list$heatmap_non_contrast, plot_list$heatmap_contrast, nrow = 1, align = "v"))

    # sapply( split(table, by = "comp_name"), function(subdata){
    #   gg_begin <- ggplot(subdata, aes(col = pval_adj < input$num_Pvalue, shape = pval_adj < input$num_Pvalue)) +
    #     ggtitle(subdata[, unique(comp_name)]) +
    #     scale_color_manual(name = paste("adj PValue <", input$num_Pvalue), values = c("blue", "red")) +
    #     scale_shape_manual(name = paste("adj PValue <", input$num_Pvalue), values = c(16, 3))
    #
    #   plot_grid(gg_begin + geom_point(aes(x = AveLogCPM, y = logFC)),
    #             gg_begin + geom_point(aes(x = logFC, y = -log10(pval_adj))) + ylab("-log10(adj PValue)"),
    #             nrow = 1, align = "v")
    # }, simplify = F)
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


    output$plot_COMP <- renderPlot({
      a <- gg_begin + geom_point(aes(x = AveLogCPM, y = logFC))
      b <- gg_begin + geom_point(aes(x = logFC, y = -log10(pval_adj))) + ylab("-log10(adj PValue)")

      plot_grid(a, b, nrow = 1, align = "v")
    })
  }, ignoreInit = T)






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



  # PCA box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_PCA
  }, {
    toggleElement("box_PCA", condition = "PCA" %in% input$chkgrp_TOOLS)

    if ("PCA" %in% input$chkgrp_TOOLS) {
      ana_object$pca <- pca_analysis(mat_res(), pca = ana_object$pca$pca, radius = input$num_PCA)

      # the plot
      output$plot_PCA <- renderPlot({
        plot_grid(ana_object$pca$corcircle, ana_object$pca$axis, nrow = 1)
      })

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

    ana_object$tsne <- tsne_analysis(data = mat_res, tsne = ana_object$tsne$tsne, epsilon = num_DBSCAN_EPSILON, minpts = num_DBSCAN_MIN)

    output$plot_tSNE <- renderPlot(ana_object$tsne$plot)

    output$txt_tSNE <- renderText(outliers_number(sum(ana_object$tsne$scan$cluster == 0)))

    if (!"tSNE" %in% input$chkgrp_TOOLS) ana_object$tsne <- NULL
  }, ignoreNULL = F, ignoreInit = T)



  # DBSCAN box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_DBSCAN_EPSILON
    input$num_DBSCAN_MIN
  }, {
    toggleElement("box_DBSCAN", condition = "DBSCAN" %in% input$chkgrp_TOOLS)

    if ("DBSCAN" %in% input$chkgrp_TOOLS && (is.null(ana_object$dbscan) || ana_object$dbscan$eps != input$num_DBSCAN_EPSILON || ana_object$dbscan$minPts != input$num_DBSCAN_MIN)) {
      ana_object$dbscan <- dbscan::dbscan(mat_res(), eps = input$num_DBSCAN_EPSILON, minPts = input$num_DBSCAN_MIN)


      output$txt_DBSCAN <- renderText({
        paste0(outliers_number(sum(ana_object$dbscan$cluster == 0)), "\n", ana_object$dbscan)
      })
    }

    if (!"DBSCAN" %in% input$chkgrp_TOOLS) ana_object$dbscan <- NULL
  }, ignoreNULL = F, ignoreInit = T)


  # ABOD box
  observeEvent({
    input$chkgrp_TOOLS
    input$num_ABOD_KNN
    input$num_ABOD_QUANTILE
  }, {
    toggleElement("box_ABOD", condition = "ABOD" %in% input$chkgrp_TOOLS)

    if ("ABOD" %in% input$chkgrp_TOOLS && (is.null(ana_object$abod) || ana_object$abod$k != input$num_ABOD_KNN)) {
      ana_object$abod <- list()
      ana_object$abod$abod <- abodOutlier::abod(mat_res(), method = "knn", k = input$num_ABOD_KNN)
      ana_object$abod$k <- input$num_ABOD_KNN
    }

    # if(!is.null(ana_object$abod)){
    #   ana_object$abod$result =
    # }
  }, ignoreNULL = F, ignoreInit = T)
}
