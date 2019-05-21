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
  contrast = reactiveVal()

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
      showElement("box_FIELDS")
      showElement("box_PARAM")
      showElement("box_CONTRAST")

      # show the content of the table
      output$table_DATASET <- renderDataTable({
        datatable(tmp,
          rownames = F, extensions = c("FixedColumns", "Scroller"),
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1),
            deferRender = TRUE,
            scrollY = 200,
            scroller = TRUE,
            searchHighlight = TRUE
          )
        )
      })
      output$txt_COLNAMES <- renderText(paste(colnames(tmp)[-1], collapse = ", "))

      param$groups <- rep("un", ncol(tmp) - 1)
      param$conditions <- rep(paste0("cond", 1:4), each = (ncol(tmp) - 1) / 4, length.out = ncol(tmp) - 1)


      output$table_GRP <- renderDataTable({
        DT <- matrix(param$groups, nrow = 1)

        datatable(DT,
          selection = "none", editable = "cell", rownames = F, colnames = names(inv())[-1], extensions = "AutoFill",
          autoHideNavigation = T, options = list(
            dom = "ft",
            scrollX = TRUE,
            searchHighlight = TRUE,
            autoFill = TRUE
          )
        )
      })

      output$table_COND <- renderDataTable({
        DT <- matrix(param$conditions, nrow = 1)

        datatable(DT,
          selection = "none", editable = "cell", extensions = "AutoFill", rownames = F, colnames = names(inv())[-1],
          autoHideNavigation = T, options = list(
            dom = "ft",
            scrollX = TRUE,
            searchHighlight = TRUE,
            autoFill = TRUE
          )
        )
      })






      # For the contrast table
      condition <- unique(param$conditions)
      DT <- diag(nrow = length(condition) - 1, ncol = length(condition))
      DT[row(DT) == col(DT) - 1] <- -1

      colnames(DT) <- condition
      DT <- data.table(
        comparison_names = rev(rev(paste(condition, shift(condition, -1), sep = "_VS_"))[-1]),
        DT
      )

      param$contrast = DT

      contrast( DT )

      output$table_CONTRAST <- renderDataTable({
        datatable(contrast(), editable = "cell", rownames = F, extensions = "Scroller", options = list(
          dom = "ft",
          scrollX = TRUE,
          searchHighlight = TRUE,
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE
        ))
      })
    }
  })

  proxy_GRP <- dataTableProxy("table_GRP")
  proxy_COND <- dataTableProxy("table_COND")

  observe({
    output$txt_GRP <- renderTable(table(param$groups))
    output$txt_COND <- renderTable(table(param$conditions))
  })



  # import the hypothetic parameters files
  observeEvent(input$file_PARAM, {

    # read the json parameters
    tmp <- read_parameter_file(input$file_PARAM$datapath)

    sapply(names(tmp), function(x) {
      param[[x]] <<- tmp[[x]]
    })

    replaceData(proxy_GRP, matrix(tmp$groups, nrow = 1, dimnames = list(NULL, names(inv())[-1])), resetPaging = F)
    replaceData(proxy_COND, matrix(tmp$conditions, nrow = 1, dimnames = list(NULL, names(inv())[-1])), resetPaging = F)

    contrast(param$contrast)
  })

  # If the user modify a cell in the group section
  observeEvent(input$table_GRP_cell_edit, {
    info <- input$table_GRP_cell_edit
    param$groups[info$col] <- as.character(info$value)
  })

  # If the user modify a cell in the condition section
  # TODO modifier cette section pour que cela renvoi directement sur le tableau des contrasts
  observeEvent(input$table_COND_cell_edit, {
    info <- input$table_COND_cell_edit
    param$conditions[info$col] <- as.character(info$value)

    set(param$contrast, NULL, setdiff(param$conditions, names(param$contrast)), 0)

    param$contrast[, setdiff(names(param$contrast), param$conditions) := NULL, with = F]

    print(param$contrast)

  })

  # if the user modify a cell in the contrast sesction
  observeEvent(input$table_CONTRAST_cell_edit, {
    info = input$table_CONTRAST_cell_edit
    set(param$contrast, as.integer(info$row), as.integer(info$col), as.integer(info$value))
    contrast(param$contrast)

  })

  # the user want to add a row
  observeEvent(input$but_ADD_ROW, {

    row_to_add = list(paste0("comparison_", nrow(param$contrast) + 1))
    row_to_add = c(row_to_add, lapply(2:ncol(param$contrast), function(x) 0))
    names(row_to_add) = names(param$contrast)
    param$contrast = rbind(param$contrast, row_to_add)

    contrast(param$contrast)

  }, ignoreInit = T)

  # the user want to delete rows
  observeEvent(input$but_DEL_ROW, {

    if (!is.null(input$table_CONTRAST_rows_selected)){
      param$contrast = param$contrast[-as.numeric(input$table_CONTRAST_rows_selected)]
    }
    contrast(param$contrast)

  })

  #
  #   # show the table of parameters on the column names like the groups and the conditions
  #   observeEvent({
  #     param
  #     input$txt_GRP
  #     input$txt_COND
  #   }, {
  #     tmp <- param()
  #
  #     # update groups and conditions in the table if that change in the text input
  #     tmp$groups <- unlist(strsplit(gsub(" ", "", input$txt_GRP), ","))
  #     tmp$conditions <- unlist(strsplit(gsub(" ", "", input$txt_COND), ","))
  #
  #
  #     # output$table_SUMMARY_column <- renderDataTable({
  #     #   DT <- data.table(
  #     #     column_names = names(inv())[-1],
  #     #     groups = tmp$groups,
  #     #     conditions = tmp$conditions
  #     #     # modify = paste0('
  #     #     #        <div class="btn-group" role="group" aria-label="Basic example">
  #     #     #        <button type="button" class="btn btn-secondary modify"id=modify_', 1:length(tmp$conditions), ">Modify</button>
  #     #     #        </div>
  #     #     #        ")
  #     #   )
  #     #   datatable(DT) # escape = c(4))
  #     # })
  #
  #     param(tmp)
  #   })
  #
  #   observeEvent({
  #     param
  #     input$txt_COND
  #     input$txtarea_CONTRAST
  #   }, {
  #     tmp <- param()
  #
  #     tmp$constrast <- if (is.null(input$txtarea_CONTRAST) || input$txtarea_CONTRAST == "") {
  #       tmp$contrast
  #     } else {
  #       fread(text = input$txtarea_CONTRAST)
  #     }
  #
  #     param(tmp)
  #
  #     output$table_SUMMARY_contrast <- renderDataTable(tmp$contrast)
  #   })
  #
  #   observe({
  #     toggleElement("but_DATASET", condition = (input$txt_GRP != "" && input$txt_COND != "" && input$txtarea_CONTRAST != ""))
  #   })




  # modal_modify<-modalDialog(
  #   fluidPage(
  #     h3(strong("Row modification"),align="center"),
  #     hr(),
  #     dataTableOutput('row_modif'),
  #     actionButton("save_changes","Save changes"),
  #
  #     tags$script(HTML("$(document).on('click', '#save_changes', function () {
  #                        var list_value=[]
  #                        for (i = 0; i < $( '.new_input' ).length; i++)
  #                        {
  #                        list_value.push($( '.new_input' )[i].value)
  #                        }
  #                        Shiny.onInputChange('newValue', list_value)
  #                        });"))
  #   ),
  #   size="l"
  # )
  #
  # observeEvent(input$lastClick, {
  #                if (input$lastClickId%like%"modify") showModal(modal_modify)
  #              }
  # )
  #
  # output$row_modif<-renderDataTable({
  #   selected_row=as.numeric(gsub("modify_","",input$lastClickId))
  #
  #   tmp <- param()
  #
  #   DT = data.table(column_names = names(inv())[-1],
  #                   groups = tmp$groups,
  #                   conditions = tmp$conditions)
  #
  #   old_row=DT[selected_row]
  #   print(old_row)
  #   row_change=list()
  #
  #   for (i in colnames(old_row))
  #   {
  #
  #     if(i %like% "names"){
  #       row_change[[i]] = as.character(old_row[, i, with = F])
  #       next
  #     }
  #
  #     row_change[[i]]<-paste0('<input class="new_input" type="text" id=new_',i ,', value = ', as.character(old_row[, i, with = F]),'><br>')
  #   }
  #   row_change=as.data.table(row_change)
  #   setnames(row_change,colnames(old_row))
  #   DT=rbind(old_row,row_change)
  #   rownames(DT)<-c("Current values","New values")
  #   DT
  #
  # },escape=F,options=list(dom='t',ordering=F),selection="none"
  # )


  # TODO Maybe
  # observeEvent(input$num_NBGROUP, {
  #   output$ui_GROUP <- renderUI({
  #     code <- lapply(seq_len(input$num_NBGROUP), function(x) {
  #       names <- paste0("_GRP_", x)
  #       label <- paste("Group nb", x)
  #
  #       list(
  #         p(label),
  #         textInput(paste0("txt", names), NULL, value = input[[paste0("txt", names)]]),
  #         selectInput(paste0("sel", names), NULL, choices = names(inv())[-1], multiple = T, selected = input[[paste0("sel", names)]])
  #       )
  #     })
  #     return(code)
  #   })
  # })
  #
  # observe({
  #
  #
  # })
}
