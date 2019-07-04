#' PCA
#'
#' Do the analysis + the graph for the pca
#'
#' @param data the data to to the pca analysis
#' @param pca the pca if it exist to not recalculate them
#' @param axis1 the number of the axis 1
#' @param axis2 the number of the axis 2
#' @param radius the radius of the circle
#'
#' @return
#' @export
#'
#' @importFrom ade4 dudi.pca
#' @importFrom ggforce geom_circle
#' @importFrom ggplot2 ggplot geom_segment geom_label coord_fixed xlab ylab ggtitle theme_gray scale_color_manual scale_shape_manual qplot arrow
#'
#' @examples
#' ""
pca_analysis <- function(data, pca = NULL, axis1 = 1, axis2 = 2, radius = 0) {
  if (is.null(pca)) {
    pca <- dudi.pca(data, center = F, scale = F, scannf = F, nf = 5)
  }

  # for the sphere
  sphere <- function(x, radius) {
    center <- apply(x, 2, mean)

    vec_dist <- rowSums(sweep(x, 2, center, "-")^2) >= radius^2

    return(vec_dist)
  }

  title_axis <- sprintf("axis %s - axis %s", axis1, axis2)

  color_circle <- "firebrick"

  corcircle <- ggplot(data = NULL, aes(x = pca$c1[, axis1], y = pca$c1[, axis2], label = rownames(pca$c1))) +
    geom_vline(xintercept = 0, color = color_circle) + geom_hline(yintercept = 0, color = color_circle) +
    geom_segment(aes(xend = 0, yend = 0), arrow = arrow(ends = "first", length = unit(0.25, "cm"))) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = F, color = color_circle) +
    geom_label(aes(vjust = ifelse(pca$c1[, axis2] < 0, "top", "bottom"))) +
    coord_fixed() + labs(x = NULL, y = NULL, title = "corcircle", subtitle = title_axis) +
    theme_gray()


  color_axis <- sphere(pca$l1, radius)
  axis <- qplot(
    x = pca$l1[, axis1], y = pca$l1[, axis2],
    color = as.character(color_axis),
    shape = as.character(color_axis)
  ) +
    scale_color_manual(name = "Outliers", values = color_true_false) +
    scale_shape_manual(name = "Outliers", values = shape_true_false) +
    labs(x = paste("axis", axis1), y = paste("axis", axis2), title = title_axis)
  theme_gray()


  return(list(pca = pca, corcircle = corcircle, axis = axis, result = color_axis))
}








#' tsne analysis
#'
#' @param data the data set
#' @param tsne if exist don't reculate it
#' @param scan if exist the dbscan of the tsne
#' @param epsilon for the dbscan
#' @param minpts for the minpts
#'
#'
#' @return
#' @export
#'
#' @importFrom Rtsne Rtsne
#' @importFrom ggplot2 ggplot geom_point scale_color_discrete scale_shape_manual ggtitle xlab ylab theme_gray
#' @importFrom data.table as.data.table
#'
#' @examples
#' ""
tsne_analysis <- function(data, tsne = NULL, scan = NULL, epsilon = 0, minpts = 0) {
  if (is.null(tsne)) {
    tsne <- Rtsne(data, pca = F, normalize = F, max_iter = 1000, theta = 0)
    tsne$Y <- as.data.table(tsne$Y)
  }

  scan <- dbscan_analysis(tsne$Y, epsilon = epsilon, minpts = minpts)

  plot <- ggplot(tsne$Y, aes(V1, V2)) +
    geom_point(aes(
      color = as.character(scan$cluster),
      shape = as.character(scan$cluster == 0L)
    )) +
    scale_color_discrete(name = "Cluster") +
    scale_shape_manual(name = "Outliers", values = shape_true_false) +
    ggtitle("tSNE") + xlab("axis 1") + ylab("axis 2") +
    theme_gray()

  return(list(tsne = tsne, dbscan = scan, tsne_plot = plot))
}




#' dbscan analysis
#'
#' @param data the data we want
#' @param scan the dbscan if it exist
#' @param epsilon for the dbscan
#' @param minpts for the dbscan
#'
#' @return
#' @export
#'
#' @importFrom dbscan dbscan
#'
#' @examples
dbscan_analysis <- function(data, scan = NULL, epsilon = 0, minpts = 0) {
  if (is.null(scan) || any(c(epsilon, minpts) != c(scan$eps, scan$minPts))) {
    scan <- dbscan(data, eps = epsilon, minPts = minpts)
  }
  return(scan)
}

#' Print the dbscan
#'
#' @param scan the dbscan
#'
#' @return
#'
#' @examples
print_dbscan <- function(scan) {
  output <- capture.output(scan)
  output <- output[ c(0, 1) - length(output) ]

  return(paste(output, collapse = "\n"))
}

#' abod analysis
#'
#' @param data the data we want to analyze
#' @param abod the abod if it exist
#' @param k for the abod
#'
#' @return
#' @export
#'
#' @importFrom abodOutlier abod
#'
#' @examples
abod_analysis <- function(data, abod = NULL, k = 15) {
  if (is.null(abod) || k != abod$k) {
    abod <- list()
    capture.output(abod$abod <- suppressWarnings(abod(data, method = "knn", k = k)))
    abod$k <- k
  }

  return(abod)
}



#' isolation forest
#'
#' @param data for the analize
#' @param isofor the object if it exist to not
#' @param nTrees see [isofor::iForest()]
#' @param phi see [isofor::iForest()]
#'
#' @return
#' @export
#'
#' @importFrom isofor iForest
#'
#' @examples
isofor_analysis <- function(data, isofor = NULL, nTrees = 100, phi = 8) {
  if (is.null(isofor) || any(c(nTrees, phi) != c(isofor$nTrees, isofor$phi))) {
    isofor <- iForest(data, nTrees, phi)
    isofor <- list(isofor = predict(isofor, data), nTrees = nTrees, phi = phi)
  }

  return(isofor)
}




#' self organizing map analysis
#'
#' @param data
#' @param som
#'
#' @return
#' @export
#'
#' @importFrom kohonen somgrid som unit.distances object.distances getCodes
#' @importFrom data.table := as.data.table melt
#' @importFrom ggplot2 ggplot geom_label scale_fill_gradientn theme_void theme ggtitle geom_col coord_polar facet_wrap
#' @importFrom ggplot2 scale_fill_manual element_blank element_text
#' @importFrom ggforce geom_circle
#'
#' @examples
som_analysis <- function(data, som = NULL) {

  if (is.null(som)) {
    som <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal", neighbourhood.fct = "gaussian")
    som <- som(data, grid = som, rlen = 200)
  }

  # take the som and return the mean of the distance between each neurone
  neigh.dist <- function(som) {
    nhbrdist <- unit.distances(som$grid)
    cddist <- as.matrix(object.distances(som, type = "codes"))
    cddist[abs(nhbrdist - 1) > 0.001] <- NA

    return(colMeans(cddist, na.rm = TRUE))
  }

  # create the theme for the plot
  theme_som <- function(gg) {
    gg + geom_circle() +
      geom_label(aes(x = x, y = y), fill = "white") +
      scale_fill_gradientn(colors = coolBlueHotRed(100)) +
      theme_void(base_size = 15) +
      theme(
        legend.box.margin = margin(r = 10),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 1)
      )
  }

  pred <- predict(som)

  data_som <- as.data.table(expand.grid(x = seq(som$grid$xdim), y = seq(som$grid$ydim)), keep.rownames = T)
  data_som[as.data.table(table(pred$unit.classif), keep.rownames = T), on = c("rn" = "V1"), N := i.N]
  data_som[, dist := neigh.dist(som)]

  count_g <- theme_som(ggplot(data_som, aes(x0 = x, y0 = y, fill = N, r = 0.5, label = rn)) +
    ggtitle("Counts plot"))

  dist_g <- theme_som(ggplot(data_som, aes(x0 = x, y0 = y, fill = dist, r = 0.5, label = rn)) +
    ggtitle("Neighbour distance plot"))


  codes = as.data.table(getCodes(som_model), keep.rownames = T)
  codes[, rn := gsub("V", "", rn)]
  codes = melt(codes, id.vars = 1)

  codes_g <- ggplot(codes, aes(x = "", y = value, group = variable, color = variable, fill = variable)) +
    geom_col(width = 1) +
    coord_polar("y") + facet_wrap(~ as.integer(rn), nrow = som$grid$xdim, ncol = som$grid$ydim, as.table = F) +
    theme_void(base_size = 15) + ggtitle("Codes plot") +
    scale_fill_manual(name = NULL, values = coolBlueHotRed(codes[, length(unique(variable))]), aesthetics = c("colour", "fill")) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 1))

  return(list(som = som, count = count_g, dist = dist_g, codes = codes_g, data = data_som, pred = pred$unit.classif))
}
