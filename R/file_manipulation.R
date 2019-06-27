#' Read the parameter file
#'
#' @param name_file path to the file we went to read
#'
#' @return
#' @export
#'
#' @importFrom jsonlite read_json
#' @importFrom data.table as.data.table
#'
#' @examples
read_parameter_file <- function(name_file) {
  data_parameters <- read_json(name_file, simplifyVector = T)

  data_parameters$contrast <- as.data.table(data_parameters$contrast)

  return(data_parameters)
}


#' Write the parameter file
#'
#' @param param param we want to write
#' @param name_file the name of the file
#'
#' @return
#' @export
#'
#' @importFrom jsonlite write_json
#'
#' @examples
write_parameter_file = function(param, name_file){

  write_json(param, name_file, dataframe = "columns", pretty = T)

}



#' put list of ggplot in a directory by names
#'
#' @param plot_list the plot list
#' @param path the path you want to save the plot
#'
#' @return nothing
#' @export
#'
#' @importFrom ggplot2 ggsave is.ggplot
#'
plot_list_save = function(plot_list, path){

  if (is.null(plot_list)) return(invisible())
  if (missing(path)) path = tempdir()

  dir.create(path, showWarnings = F, recursive = T)
  for (i in seq_along(plot_list)){
    if( is.ggplot(plot_list[[i]]) ){
      ggsave(paste0(names(plot_list)[i], ".png"), plot_list[[i]], path = path)
    }
  }
  invisible()
}



#' save the pca
#'
#' @param pca
#' @param radius
#' @param path
#'
#' @return the result of the analysis
#' @export
#'
#' @importFrom ggplot2 ggsave
pca_save = function(pca = NULL, radius = 0, path){

  if (is.null(pca)) return(NULL)
  dir.create(path, showWarnings = F, recursive = T)

  combi = combn(1:pca$nf, 2)
  sapply(1:ncol(combi), function(x){
    title_partial = paste(combi[1, x], combi[2, x], sep = "_")
    tmp = pca_analysis(pca$tab, pca, combi[1, x], combi[2, x], radius)
    ggsave(paste0("pca_corcircle_axis_", title_partial, ".png"), tmp$corcircle, path = path)
    ggsave(paste0("pca_axis_", title_partial, ".png"), tmp$axis, path = path)

    invisible()
  })

  return(pca_analysis(pca$tab, pca, radius = radius)$result)
}



