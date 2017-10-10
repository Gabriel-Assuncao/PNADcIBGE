#' Path for example data
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' pnadc_example()
#' pnadc_example("exampledata.txt")

pnadc_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "PNADcIBGE"))
  } else {
    system.file("extdata", path, package = "PNADcIBGE", mustWork = TRUE)
  }
}
