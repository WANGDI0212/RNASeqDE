twoPlot_server <- function(input, output, session, plot1, plot2) {
  observe({
    if (is.ggplot(plot1()) && is.ggplot(plot2())) {
      output$plot <- renderPlot(plot_grid(plot1(), plot2(), nrow = 1, align = "v"))
    } else {
      output$plot <- renderPlot("error")
    }
  })
}

comparison_box_server <- function(input, output, session, data, plot_list) {
  observeEvent(ignoreInit = T, input$pvalue, {

      # update the table with the Pvalue you want
      output$table <- renderTable({
        data()[pval_adj <= input$pvalue, .(total = .N, up = sum(logFC > 0), down = sum(logFC < 0)), by = comp_name ]
      })


      # update the heatmap
      # keep the rows that have at least one of the Pvalue is down to the Pvalue they want
      keep.row <- data()[, sum(pval_adj <= input$pvalue) >= 1, by = rn][(V1)]
      if (nrow(keep.row) >= 1) {
        plot_list$heatmap_non_contrast <- heatmap_ggplot(data()[
          keep.row,
          on = "rn"
        ], "heatmap non contrasted")

        plot_list$heatmap_contrast <- heatmap_ggplot(
          data()[pval_adj <= input$pvalue],
          "heatmap contrasted"
        )
      }
      output$plot_HEATMAP <- renderPlot(plot_grid(plot_list$heatmap_non_contrast, plot_list$heatmap_contrast, nrow = 1, align = "v"))
  })

  observeEvent(ignoreInit = T, {
    input$comparison
    input$pvalue
    }, {

      # to avoid rewrite the same lines over and over
      name_legend <- paste("adj PValue <=", input$pvalue)
      gg_begin <- ggplot(data()[comp_name == input$comparison], aes(
        col = as.character(pval_adj < input$pvalue),
        shape = as.character(pval_adj < input$pvalue)
      )) +
        ggtitle(input$comparison) +
        scale_color_manual(name = "Outliers", values = color_true_false) +
        scale_shape_manual(name = "Outliers", values = shape_true_false) + theme_gray()

      plot_list$Smear_plot <- gg_begin + geom_point(aes(x = AveLogCPM, y = logFC)) + geom_smooth(aes(x = AveLogCPM, y = logFC))
      plot_list$volcano_plot <- gg_begin + geom_point(aes(x = logFC, y = -log10(pval_adj))) + ylab("-log10(adj PValue)")

  })

  return(plot_list)
}
