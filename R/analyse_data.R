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
#' @importFrom ggplot2 ggplot geom_segment geom_label coord_fixed xlab ylab ggtitle theme_gray scale_color_manual scale_shape_manual qplot
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

  corcircle <- ggplot(data = NULL, aes(x = pca$c1[, axis1], y = pca$c1[, axis2], label = rownames(pca$c1))) +
    geom_segment(aes(xend = 0, yend = 0), arrow = arrow(ends = "first", length = unit(0.25, "cm"))) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = F) +
    geom_label(aes(vjust = ifelse(pca$c1[, axis2] < 0, "top", "bottom"))) +
    coord_fixed() + xlab(NULL) + ylab(NULL) + ggtitle("corcircle", title_axis) + theme_gray()


  color_axis <- sphere(pca$l1, radius)
  axis <- qplot(
    x = pca$l1[, axis1], y = pca$l1[, axis2],
    main = "The first two axis",
    color = as.character(color_axis),
    shape = as.character(color_axis)
  ) +
    scale_color_manual(name = "Outliers", values = color_true_false) +
    scale_shape_manual(name = "Outliers", values = shape_true_false) +
    ggtitle(title_axis)
  theme_gray()


  return(list(pca = pca, corcircle = corcircle, axis = axis, result = color_axis))
}








#' tsne analysis
#'
#' @param data the data set
#' @param tsne if exist don't reculate it
#' @param epsilon for the dbscan
#' @param minpts for the minpts
#'
#' @return
#' @export
#'
#' @importFrom Rtsne Rtsne
#' @importFrom dbscan dbscan
#' @importFrom ggplot2 ggplot geom_point scale_color_discrete scale_shape_manual ggtitle xlab ylab theme_gray
#' @importFrom data.table as.data.table
#'
#' @examples
#' ""
tsne_analysis <- function(data, tsne = NULL, epsilon = 0, minpts = 0) {
  if (is.null(tsne)) {
    tsne <- Rtsne(data, pca = F, normalize = F, max_iter = 1000, theta = 0)
    tsne$Y <- as.data.table(tsne$Y)
  }

  scan <- dbscan(tsne$Y, eps = epsilon, minPts = minpts)

  plot <- ggplot(tsne$Y, aes(V1, V2)) +
    geom_point(aes(
      color = as.character(scan$cluster),
      shape = as.character(scan$cluster == 0)
    )) +
    scale_color_discrete(name = "Cluster") +
    scale_shape_manual(name = "Outliers", values = shape_true_false) +
    ggtitle("tSNE") + xlab("axis1") + ylab("axis 2") +
    theme_gray()

  return(list(tsne = tsne, scan = scan, plot = plot))
}
