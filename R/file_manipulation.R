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
  data_parameters <- read_json(name_file, simplifyVector = T)

  data_parameters$contrast <- as.data.table(data_parameters$contrast)

  return(data_parameters)
}
