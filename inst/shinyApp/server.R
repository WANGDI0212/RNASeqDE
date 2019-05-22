function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_RES")
  })


  # load dataset ------------------------------------------------------------

  inv <- reactiveVal()
  param <- reactiveValues()
  contrast <- reactiveVal()

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
        dec = input$rad_decimal
      ))

      inv(tmp)

      # show the box
      showElement("box_DATASET")
      showElement("box_PARAM")
      showElement("but_DATASET")



      # show the content of the table
      output$table_DATASET <- renderRHandsontable({
        rhandsontable(head(tmp, n = 100), stretchH = "all", height = 250, readOnly = T) %>% hot_cols(fixedColumnsLeft = 1)
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

  #
  observe({
    if (!is.null(input$table_CONTRAST)) {
      param$contrast <- hot_to_r(input$table_CONTRAST)
    }
  })


  # the tables
  output$table_GRP <- renderRHandsontable({
    rhandsontable(param$groups, stretchH = "all")
  })

  output$table_COND <- renderRHandsontable({
    rhandsontable(param$conditions, stretchH = "all")
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

}
