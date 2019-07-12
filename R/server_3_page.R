#' @importFrom ade4 dudi.pca
#' @importFrom cowplot plot_grid
PCA_box_server <- function(input, output, session, analysis, data, update) {
  observeEvent({
    update
    data
  }, {
    print("Bonjour")

    if (update()) {
      analysis$pca$pca <- dudi.pca(data(), center = F, scale = F, scannf = F, nf = 5)
      output$inertia <- renderText({
        with(
          analysis$pca$pca,
          paste("The cumulative percentage inertia of the pca are :\n", paste(cumsum(eig) / sum(eig) * 100, collapse = " "))
        )
      })
    }
    else {
      analysis$pca <- NULL
    }
  },
  ignoreInit = T
  )

  # if there the pca change or the input sphere radius (if it not NA)
  observeEvent({
    analysis$pca$pca
    if (!is.na(input$sphere_radius)) {
      input$sphere_radius
    }
  }, {
    # draw the plot the result of the pca
    analysis$pca <- c(pca = pca, pca_analysis(pca = analysis$pca$pca, radius = input$sphere_radius))

    # make the plot
    output$plot <- plot_grid(plotlist = analysis$pca$plot, align = "h", nrow = 1)

    #
    output$outliers <- renderText(outliers_number(sum(analysis$pca$result)))
  }, ignoreInit = T, ignoreNULL = T)

  return(analysis)
}
