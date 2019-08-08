#' @importFrom ade4 dudi.pca
#' @importFrom cowplot plot_grid
PCA_box_server <- function(input, output, session, data, update) {
  ana <- reactiveValues()

  # update the analysis
  observeEvent({
    update()
    data()
  }, {
    if (update()) {
      ana$pca <- dudi.pca(data(), center = F, scale = F, scannf = F, nf = 5)
      output$inertia <- renderText({
        paste(
          "The cumulative percentage inertia of the pca are :\n\n",
          paste(round(cumsum(ana$pca$eig) / sum(ana$pca$eig) * 100, 3),
            collapse = " "
          )
        )
      })
    }
  },
  ignoreInit = T
  )

  # update the plot
  # if there the pca change or the input sphere radius (if it not NA)
  observeEvent({
    ana$pca
    if (!is.na(input$sphere_radius)) {
      input$sphere_radius
    }
  }, {
    if (!is.null(ana$pca)) {
      # draw the plot the result of the pca
      ana$res <- pca_analysis(pca = ana$pca, radius = input$sphere_radius)

      # make the plot
      output$plot <- renderPlot(if (!is.null(ana$res$plot)) plot_grid(plotlist = ana$res$plot, align = "h", nrow = 1))

      # Render the text
      output$outliers <- renderText(outliers_number(sum(ana$res$result)))
    }
  }, ignoreInit = T, ignoreNULL = F)

  return(ana)
}



#' @importFrom Rtsne Rtsne
#' @importFrom data.table as.data.table
tSNE_box_server <- function(input, output, session, data, update) {
  ana <- reactiveValues()

  observeEvent({
    update()
    data()
  }, {
    if (update()) {
      ana$tsne <- Rtsne(data(), pca = F, normalize = F, max_iter = 1000, theta = 0)
      ana$tsne$Y <- as.data.table(ana$tsne$Y)
    }
  },
  ignoreInit = T
  )

  # if there the pca change or the input sphere radius (if it not NA)
  observeEvent({
    ana$tsne
    input$"dbscan-epsillon"
    input$"dbscan-minPts"
    input$"dbscan-k"
    input$"dbscan-mean"
  }, {
    if (!is.null(ana$tsne) && !is.na(input$"dbscan-epsillon") && input$"dbscan-epsillon" > 0) {
      ana$dbscan <- dbscan(ana$tsne$Y, eps = input$"dbscan-epsillon", minPts = input$"dbscan-minPts")
      output$"dbscan-result" <- renderText(print_dbscan(ana$dbscan))
      ana$plot_dbscan_tsne <- kNNdistplot(ana$tsne$Y, k = input$"dbscan-k", eps = input$"dbscan-epsillon", meanDist = input$"dbscan-mean")
      output$"dbscan-plot" <- renderPlot(ana$plot_dbscan_tsne)
    }
  })

  observeEvent({
    ana$dbscan
  }, {
    if (is.null(ana$dbscan)) {
      ana$plot_tsne <- NULL
    } else {
      # ana$dbscan <- dbscan(ana$tsne$Y, input$"dbscan-epsillon", input$"dbscan-minPts")
      # output$"dbscan-result" <- renderText(print_dbscan(ana$dbscan))

      # draw the plot the result of the tsne
      ana$plot_tsne <- tsne_analysis(tsne = ana$tsne, scan = ana$dbscan)

      # make the plot
      output$plot <- renderPlot(ana$plot_tsne)

      # Render the text
      output$outliers <- renderText(outliers_number(sum(ana$dbscan$cluster == 0)))
    }
  }, ignoreInit = T, ignoreNULL = F)

  return(ana)
}



#' update_change_value
#'
#' To update the value if that change, return true if one value change.
#'
#' @importFrom purrr map_lgl compact
update_change_value <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(T)
  }

  # identify and nullify the NULL elements
  y <- y[ x %>% map_lgl(~ !is.null(.)) ]
  x <- x[ y %>% map_lgl(~ !is.null(.)) ]

  x <- compact(x)
  y <- compact(y)

  if (length(x) == 0) {
    return(T)
  }

  if (anyNA(x) || anyNA(y))
    return(F)

  return(any(x != y))
}


#' @importFrom dbscan dbscan
DBSCAN_box_server <- function(input, output, session, data, update) {
  ana <- reactiveValues()

  observeEvent({
    update()
    data()
    input$epsillon
    input$minPts
    input$k
    input$mean
  },{
    if (update() || update_change_value(
      c(ana$dbscan$eps, ana$dbscan$minPts, ana$dbscan$k, ana$dbscan$mean),
      c(input$epsillon, input$minPts, input$k, input$mean)
    )) {
      ana$dbscan <- dbscan(data(), input$epsillon, input$minPts)
      ana$dbscan$k <- input$k
      ana$dbscan$meanDist <- input$mean
      output$result <- renderText(print_dbscan(ana$dbscan))
      ana$plot <- kNNdistplot(data(), k = input$k, eps = input$epsillon, meanDist = input$mean)
      output$plot <- renderPlot(ana$plot)
    }
  }, ignoreInit = T)

  return(ana)
}

#' @importFrom abodOutlier abod
ABOD_box_server <- function(input, output, session, data, update) {
  ana <- reactiveValues()

  observeEvent({
    input$knn
    update()
    data()
  }, {
    if (update() || update_change_value(ana$knn, input$knn)) {
      capture.output(ana$abod <- suppressWarnings(abod(data(), method = "knn", k = input$knn)))
      ana$knn <- input$knn
    }
  }, ignoreInit = T)

  observeEvent({
    input$"quantile-quantile"
    ana$abod
  }, {
    ana$res <- ana$abod < quantile(ana$abod, input$"quantile-quantile")
    output$outliers <- renderText(outliers_number(sum(ana$res)))
  }, ignoreInit = T)

  return(ana)
}



#' @importFrom isofor iForest
ISOFOR_box_server <- function(input, output, session, data, update) {
  ana <- reactiveValues()

  observeEvent({
    input$tree_depth
    input$trees_number
    update()
    data()
  }, {
    if (update() || update_change_value(c(ana$trees_number, ana$tree_depth), c(input$trees_number, input$tree_depth))) {
      isofor <- iForest(data(), as.integer(input$trees_number), 2^as.integer(input$tree_depth))
      ana$isofor <- predict(isofor, data())
      ana$trees_number <- input$trees_number
      ana$tree_depth <- input$tree_depth
    }
  }, ignoreInit = T)

  observeEvent({
    input$"quantile-quantile"
    ana$isofor
  }, {
    if (!is.null(ana$isofor)) {
      ana$res <- ana$isofor > quantile(ana$isofor, input$"quantile-quantile")
      output$outliers <- renderPrint(
        cat("The summary of the analysis : ",
          paste(capture.output(summary(ana$isofor)), collapse = "\n"),
          outliers_number(sum(ana$res)),
          sep = "\n\n"
        )
      )
    }
  }, ignoreInit = T)

  return(ana)
}


SOM_box_server <- function(input, output, session, data, update) {
  ana <- reactiveValues()

  observeEvent({
    update()
    data()
  }, {
    if (update()) {
      ana$som <- som_analysis(data())
      output$plot <- renderPlot({
        with(ana$som, plot_grid(plot_grid(count, dist), codes, nrow = 2, rel_heights = c(1, 2)))
      })
    }
  }, ignoreInit = T)

  observeEvent({
    ana$som
    input$"quantile-quantile"
  }, {
    # the N is the number of genes inside a neurone
    # so we pick the number of neurons
    if (!is.null(ana$som)) {
      ana$som$res <- with(ana$som, pred %in% as.numeric(data[dist > quantile(dist, input$"quantile-quantile"), rn]))
      output$outliers <- renderText(outliers_number(sum(ana$som$res)))
    }
  }, ignoreInit = T)

  return(ana)
}
