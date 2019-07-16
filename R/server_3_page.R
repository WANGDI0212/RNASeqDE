


#' @importFrom ade4 dudi.pca
#' @importFrom cowplot plot_grid
PCA_box_server <- function(input, output, session, analysis, data, update) {

  # update the analysis
  observeEvent({
    update()
    data()
  }, {
    if (update()) {
      analysis$ana$pca <- dudi.pca(data(), center = F, scale = F, scannf = F, nf = 5)
      output$inertia <- renderText({
        with(
          analysis$ana$pca,
          paste("The cumulative percentage inertia of the pca are :\n", paste(cumsum(eig) / sum(eig) * 100, collapse = " "))
        )
      })
    }
    else {
      analysis$ana$pca <- NULL
    }
  },
  ignoreInit = T
  )

  # update the plot
  # if there the pca change or the input sphere radius (if it not NA)
  observeEvent({
    analysis$ana$pca
    if (!is.na(input$sphere_radius)) {
      input$sphere_radius
    }
  }, {
    if (is.null(analysis$ana$pca)) {
      analysis$res$pca <- NULL
    } else {
      # draw the plot the result of the pca
      analysis$res$pca <- pca_analysis(pca = analysis$ana$pca, radius = input$sphere_radius)

      # make the plot
      output$plot <- renderPlot(plot_grid(plotlist = analysis$res$pca$plot, align = "h", nrow = 1))

      # Render the text
      output$outliers <- renderText(outliers_number(sum(analysis$res$pca$result)))
    }
  }, ignoreInit = T, ignoreNULL = F)

  return(analysis)
}




tSNE_box_server = function(input, output, session, analysis, data, update){

  observeEvent({
    update()
    data()
  }, {
    if (update()) {
      analysis$ana$tsne <- Rtsne(data(), pca = F, normalize = F, max_iter = 1000, theta = 0)
      analysis$ana$tsne$Y = as.data.table(analysis$ana$tsne$Y)
    }
    else {
      analysis$ana$tsne <- NULL
    }
  },
  ignoreInit = T
  )

  # if there the pca change or the input sphere radius (if it not NA)
  observeEvent({
    analysis$ana$tsne
    if (!is.na(input$sphere_radius)) {
      input$sphere_radius
    }
  }, {
    if (is.null(analysis$ana$pca)) {
      analysis$res$pca <- NULL
    } else {
      # draw the plot the result of the pca
      analysis$res$pca <- pca_analysis(pca = analysis$ana$pca, radius = input$sphere_radius)

      # make the plot
      output$plot <- renderPlot(plot_grid(plotlist = analysis$res$pca$plot, align = "h", nrow = 1))

      # Render the text
      output$outliers <- renderText(outliers_number(sum(analysis$res$pca$result)))
    }
  }, ignoreInit = T, ignoreNULL = F)

  return(analysis)

}



