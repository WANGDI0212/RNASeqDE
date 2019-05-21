# Manipulate files

# read the parameters files with json lite
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

  data_parameters = read_json(name_file, simplifyVector = T)

  data_parameters$contrast = as.data.table(data_parameters$contrast)

  return(data_parameters)
}



#' Convert a data.table in text
#'
#' @param data the data.table
#'
#' @return
#' @export
#'
#' @examples
tableToText = function(data){

  out = sapply(1:nrow(data), function(x){

    paste(as.character(data[x]), collapse = ", ")

  })

  out = c(paste(names(data), collapse = ", "), out)

  out = paste(out, collapse = "\n")

}
