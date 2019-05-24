# parameters extraction ---------------------------------------------------
# comptage matrix



# filtering the count table -----------------------------------------------

#' Filter the counting table
#'
#' Remove all the lines with at least the count per million is superior than 1 on at least 3 columns
#'
#' @param counts The counting table
#'
#' @return the matrix that had been filtered
#' @export
#'
#' @importFrom edgeR cpm
#'
#' @examples
filter <- function(counts) {
  if (!is.matrix(counts)) {
    counts <- as.matrix(counts, rownames = names(counts)[1])
  }

  counts <- na.omit(counts)
  keep.exprs <- rowSums(cpm(counts) > 1) >= 3 # we select the where the cpm is superior than 1 on at least 3 columns
  counts <- counts[keep.exprs, ] ## keep the rows selected

  return(counts)
}




# normalization -----------------------------------------------------------

#' Normalize the dataset
#'
#' @param counts matrix of the counting table
#' @param groups the groups (flowcells) for each of the column of the data
#'
#' @return a list with the normalized dataset in DGElist and the plot associated
#' @export
#'
#' @importFrom edgeR DGEList calcNormFactors
#' @importFrom limma plotMDS
#' @importFrom ggplot2 ggplot ggtitle geom_point geom_text xlab ylab scale_color_discrete aes
#'
#' @examples
normalization <- function(counts, groups) {

  # without normalization
  y <- DGEList(counts, group = groups)
  coord_raw <- plotMDS(y, main = "MDSplot on the raw dataset", plot = F)

  gg_list <- list()

  gg_list$raw <- with(coord_raw, {
    ggplot(NULL, aes(x = x, y = y, label = names(x), color = groups)) +
      ggtitle("MDSplot on the raw dataset")
  })


  # with normalization
  y <- calcNormFactors(y, method = "TMM")
  coord <- plotMDS(y, top = min(500, nrow(y$samples$norm.factors)), method = "logFC", gene.selection = "pairwise", plot = FALSE)

  gg_list$normalize <- with(coord, {
    ggplot(NULL, aes(x = x, y = y, label = names(x), color = groups)) +
      ggtitle("MDSplot on the normalized dataset")
  })


  gg_list <- lapply(gg_list, function(x) {
    x +
      geom_point() +
      geom_text(vjust = 0, nudge_y = 0.05, show.legend = F, hjust = 0.5) +
      xlab("Leading logFC dimension 1") +
      ylab("Leading logFC dimension 2") +
      scale_color_discrete(name = "groups")
  })

  return(list(normalized_dataset = y, plot = gg_list))
}



# Differential expression analysis ----------------------------------------
#' Differential expression
#'
#' @param y DGElist normalized
#' @param conditions conditions of the analysis
#'
#' @return the glm fit tagwise dispersion
#' @export
#'
#' @importFrom stats model.matrix
#' @importFrom edgeR estimateGLMCommonDisp estimateGLMTrendedDisp estimateGLMTagwiseDisp glmFit
#'
#' @examples
DE <- function(y, conditions) {
  conditions <- factor(conditions, unique(conditions), ordered = T)

  design <- model.matrix(~ 0 + conditions)

  y_tmp <- estimateGLMCommonDisp(y, design) ## Common dispersion estimation: the same dispersion value is used to model the variance of a gene
  y_tmp <- estimateGLMTrendedDisp(y_tmp, design) ## Trended dispersion estimation: esitmation with different dispersion values for each gene, to give an idea
  y_tmp <- estimateGLMTagwiseDisp(y_tmp, design) ## Tagwise to take into account a specific dispersion for each gene
  fit <- glmFit(y_tmp, design) ## Tagwise dispersion Glm

  return(fit)
}



# comparisons -------------------------------------------------------------
#' Do the comparison
#'
#' Make all the comparison that is inside the contrast arguments
#'
#' @param fit the glmfit form edgeR
#' @param contrast the contrast data.table
#'
#' @return list with 3 elements:
#'  - data, a data.table with:
#'     * rn: the name of the genes
#'     * logFC: log fold change
#'     * pval_adj: pvalue adjusted
#'     * comp_name: the name of the comparison
#'  - plot, a list with the names of the comparison, inside each of them there is another list with:
#'     * the Smear plot (mean-difference plot)
#'     * volcano plot
#' @export
#'
#' @importFrom data.table setDT := as.data.table rbindlist
#' @importFrom edgeR glmLRT
#'
#' @examples
comparison <- function(fit, contrast) {

  setDT(contrast)

  plot = list()
  table = rbindlist(lapply( split(contrast, by = names(contrast)[1], keep.by = T), function(x){

    comp_name <- as.character(x[, 1])
    comp <- glmLRT(fit, contrast = unlist(x[, 2:ncol(x)]))

    table = as.data.table(comp$table, keep.rownames = T)

    table[, ":="(pval_adj = p.adjust(PValue, "BH") )]

    return(table[, .(rn, logFC, pval_adj, logCPM, comp_name)])

  }))

  # dcast(table[pval_adj <= 0.05], rn ~ comp_name, value.var = "logFC", fill = 0)

  # heatmap = pheatmap(t(dcast(table[pval_adj <= 0.05], rn ~ comp_name, value.var = "logFC", fill = 0)), scale = "column", silent = T)

  return(table[, .(rn, logFC, pval_adj, AveLogCPM = logCPM, comp_name)])
}
