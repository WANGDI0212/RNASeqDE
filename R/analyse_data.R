

#' PCA
#'
#' Do the analysis + the graph for the pca
#'
#' @param data
#' @param pca
#'
#' @return
#' @export
#'
#' @importFrom ade4 dudi.pca
#' @importFrom ggforce geom_circle
#' @importFrom ggplot2 ggplot geom_segment geom_label coord_fixed xlab ylab ggtitle theme_gray scale_color_manual scale_shape_manual qplot
#'
#' @examples
pca_analysis = function(data, pca = NULL, axis1 = 1, axis2 = 2, radius = 0){

    if (is.null(pca)){
      pca = dudi.pca(data, center = F, scale = F, scannf = F, nf = 5)
    }

  # for the sphere
  sphere <- function(x, radius) {
    center <- apply(x, 2, mean)

    vec_dist <- rowSums(sweep(x, 2, center, "-")^2) >= radius^2

    return(as.character(vec_dist))
  }

  color_axis = sphere(pca$l1, input$num_PCA)

  axis1_c = pca$c1[, axis1]
  axis2_c = pca$c1[, axis2]

  corcircle <- ggplot(data = NULL, aes(x = axis1_c, y = axis2_c, label = rownames(pca$c1))) +
    geom_segment(aes(xend = 0, yend = 0), arrow = arrow(ends = "first", length = unit(0.25, "cm"))) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = F) +
    geom_label(aes(vjust = ifelse(axis2_c < 0, "top", "bottom"))) +
    coord_fixed() + xlab(NULL) + ylab(NULL) + ggtitle("corcircle") + theme_gray()

  axis <- qplot(
    x = pca$l1[, axis1], y = pca$l1[, axis2],
    main = "The first two axis",
    color = color_axis,
    shape = color_axis
  ) +
    scale_color_manual(name = "Outliers", values = color_true_false) +
    scale_shape_manual(name = "Outliers", values = shape_true_false) +
    theme_gray()


  return(list(pca = pca, corcircle = corcircle, axis = axis))

}

