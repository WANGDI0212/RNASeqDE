csvFile <- function(input, output, session) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  dataframe <- reactive(fread(
    file = userFile()$datapath,
    skip = ifelse(is.na(input$num_skip_line) || input$num_skip_line == 0, "__auto__", input$num_skip_line),
    data.table = T,
    dec = input$rad_decimal,
    fill = T,
    blank.lines.skip = T
  ))

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}



AfterDataset <- function(input, output, session, data) {
  param <- reactiveValues()

  observe({

    # show the content of the table
    output$table_DATASET <- renderRHandsontable({
      rhandsontable(head(data(), n = 50), stretchH = "all", height = 250, readOnly = T) %>% hot_cols(fixedColumnsLeft = 1)
    })


    # give the parameters of the groups
    param$groups <- if (is.null(param$groups)) {
      matrix(rep("un", ncol(data()) - 1),
        nrow = 1,
        dimnames = list(NULL, names(data())[-1])
      )
    } else {
      param$groups
    }

    # gives the parameters of the conditions
    param$conditions <-
      if (is.null(param$conditions)) {
        matrix(rep(paste0("cond", 1:4), each = (ncol(data()) - 1) / 4, length.out = ncol(data()) - 1),
          nrow = 1,
          dimnames = list(NULL, names(data())[-1])
        )
      } else {
        param$conditions
      }

    # give the parameters of the contrast table
    if (is.null(param$contrast)) {
      condition <- unique(as.vector(param$conditions))
      DT <- diag(nrow = length(condition) - 1, ncol = length(condition))
      DT[row(DT) == col(DT) - 1] <- -1

      colnames(DT) <- condition
      DT <- data.table(
        comparison_names = rev(rev(paste(condition, shift(condition, -1), sep = "_VS_"))[-1]),
        DT
      )

      param$contrast <- DT
    } else {
      param$contrast
    }
  })

  return(param)
}












parameterBox_server <- function(input, output, session, columns, param) {
  output$colnames <- renderText(paste(columns()[-1], collapse = ", "))


  # take the input paramters in the group table
  observe({
    if (!is.null(input$group)) {
      param$groups <- hot_to_r(input$group)
    }
  })

  # take the input paramters in the condition table
  observe({
    if (!is.null(input$condition)) {
      param$conditions <- hot_to_r(input$condition)
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
    if (!is.null(input$contrast)) {
      tmp <- hot_to_r(input$contrast)

      added_row <- unique(which(is.na(tmp), arr.ind = T)[, "row"])
      if (length(added_row) != 0) {
        set(tmp, added_row, 2:ncol(tmp), 0)
        set(tmp, added_row, 1L, basename(tempfile("comp_")))
      }
      param$contrast <- tmp
    }
  })


  # the tables
  output$group <- renderRHandsontable({
    if (is.null(param$groups)) {
      NULL
    } else {
      rhandsontable(param$groups, stretchH = "all") %>%
        hot_context_menu(allowRowEdit = F, allowColEdit = F)
    }
  })

  output$condition <- renderRHandsontable({
    if (is.null(param$conditions)) {
      NULL
    } else {
      rhandsontable(param$conditions, stretchH = "all") %>%
        hot_context_menu(allowRowEdit = F, allowColEdit = F)
    }
  })

  output$contrast <- renderRHandsontable({
    if (is.null(param$contrast)) {
      NULL
    } else {
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
    }
  })

  # to the table functions
  observe({
    output$txt_GRP <- renderTable(table(param$groups))
    output$txt_COND <- renderTable(table(param$conditions))
  })

  return(param)
}





parametersInput_server <- function(input, output, session, colname, param) {
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })

  # import the hypothetic parameters files
  observe({

    # read the json parameters
    tmp <- read_parameter_file(userFile()$datapath)

    param$groups <- matrix(tmp$groups, nrow = 1, dimnames = list(NULL, colname()[-1]))
    param$conditions <- matrix(tmp$conditions, nrow = 1, dimnames = list(NULL, colname()[-1]))

    param$contrast <- tmp$contrast
  })

  return(param)
}
