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

  # take the
  observe({
    if (!is.null(input$table_CONTRAST)) {
      tmp <- hot_to_r(input$table_CONTRAST)

      added_row = unique( which(is.na( tmp ), arr.ind = T)[, "row"] )
      if (length(added_row) != 0 ){
        set(tmp, added_row, 2:ncol(tmp), 0)
        set( tmp, added_row, 1L, basename(tempfile("comp_")))
      }
      print(tmp)
      param$contrast = tmp
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
      hot_validate_numeric(cols = 2:ncol(param$contrast))
  })

  # to the table functions
  observe({
    output$txt_GRP <- renderTable(table(param$groups))
    output$txt_COND <- renderTable(table(param$conditions))
  })





  # Result ------------------------------------------------------------------

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

    # The plot on the normalisation
    output$plot_NORM <- renderPlot(plot_grid(plot_normalization$raw, plot_normalization$normalize, nrow = 1, align = "v"))

    # update the input from the data we have
    updateSelectInput(session, "sel_COMP", choices = data_comp[, unique(comp_name)], selected = data_comp[, unique(comp_name)][1])
    updateSliderInput(session, "num_Pvalue", value = 0.05, min = 0, max = round(data_comp[, max(pval_adj)]), step = 0.001)
  })


  observeEvent(input$num_Pvalue, {
    # update the table with the Pvalue you want
    output$tab_COMP <- renderTable({
      data_comp[pval_adj < input$num_Pvalue, .(total = .N, up = sum(logFC > 0), down = sum(logFC < 0)), by = comp_name ]
    })

    # update the heatmap
    # keep the rows that have at least one of the Pvalue is down to the Pvalue they want
    keep.row <- dcast(data_comp, rn ~ comp_name, value.var = c("pval_adj"))[, rowSums(.SD <= input$num_Pvalue) >= 1, .SDcols = -1]

    if (sum(keep.row) >= 1) {
      output$plot_HEATMAP_NON_CONTRAST <- renderPlot(pheatmap(
        t(as.matrix(dcast(data_comp, rn ~ comp_name, value.var = c("logFC"))[keep.row], rownames = "rn")),
        scale = "row", main = "heatmap non contrasted", show_colnames = F
      ))

      output$plot_HEATMAP_CONTRAST <- renderPlot(pheatmap(
        t(as.matrix(dcast(data_comp[pval_adj <= input$num_Pvalue], rn ~ comp_name, value.var = c("logFC"), fill = 0), rownames = "rn")),
        scale = "row", main = "heatmap contrasted", show_colnames = F
      ))
    }
  }, ignoreInit = T)

  # update the graph
  observeEvent({
    input$sel_COMP
    input$num_Pvalue
  }, {
    gg_begin <- ggplot(data_comp[comp_name == input$sel_COMP], aes(col = pval_adj < input$num_Pvalue, shape = pval_adj < input$num_Pvalue)) +
      ggtitle(input$sel_COMP) +
      scale_color_manual(name = paste("adj PValue <", input$num_Pvalue), values = c("blue", "red")) +
      scale_shape_manual(name = paste("adj PValue <", input$num_Pvalue), values = c(16, 3))


    output$plot_COMP <- renderPlot({
      a <- gg_begin + geom_point(aes(x = AveLogCPM, y = logFC))
      b <- gg_begin + geom_point(aes(x = logFC, y = -log10(pval_adj))) + ylab("-log10(adj PValue)")

      plot_grid(a, b, nrow = 1, align = "v")
    })
  }, ignoreInit = T)


  mat_res <- reactiveVal()
  observeEvent({
    input$but_RES
    if (input$but_RES >= 1) {
      input$num_Pvalue
    }
  }, {
    showMenuItem("tab_ANA")

    mat_res(dcast(data_comp[pval_adj <= input$num_Pvalue], rn ~ comp_name, value.var = "logFC", fill = 0))

    updateTabItems(session, "mnu_MENU", "tab_ANA")
  }, ignoreInit = T)


  updatebox = c("PCA" = F, "tSNE" = F, "som" = F, "DBSCAN" = F, "ABOD" = F, "isofor" = F)
}
