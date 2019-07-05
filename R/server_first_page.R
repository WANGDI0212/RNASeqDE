csvFile <- function(input, output, session, skip_line, decimal) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  dataframe <- fread(
      file = userFile()$datapath,
      skip = ifelse(is.na(skip_line()) || skip_line() == 0, "__auto__", skip_line()),
      data.table = T,
      dec = decimal(),
      fill = T,
      blank.lines.skip = T
    )

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}



AfterDataset <- function(input, output, session, data) {

  observeEvent(ignoreInit = T, {
    input$"table-file"
    input$num_skip_line
    input$rad_decimal
  }, {

    dataframe <- callModule(csvFile, "table", reactive(input$num_skip_line), reactive(input$rad_decimal))

    data(dataframe)

    param = list()

    # show the content of the table
    output$table_DATASET <- renderRHandsontable({
      rhandsontable(head(dataframe, n = 50), stretchH = "all", height = 250, readOnly = T) %>% hot_cols(fixedColumnsLeft = 1)
    })
    output$colnames <- renderText(paste(colnames(dataframe)[-1], collapse = ", "))

    param$groups <- matrix(rep("un", ncol(dataframe) - 1),
      nrow = 1,
      dimnames = list(NULL, names(dataframe)[-1])
    )
    param$conditions <- matrix(rep(paste0("cond", 1:4), each = (ncol(dataframe) - 1) / 4, length.out = ncol(dataframe) - 1),
      nrow = 1,
      dimnames = list(NULL, names(dataframe)[-1])
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
    return(param)
  })


}
