#' Title
#'
#' taken from vignette at http://www.omegahat.net/RSXML/gettingStarted.html
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom XML xmlTreeParse
#' @importFrom XML xmlRoot
#' @importFrom magrittr::`%>%`
importBXML <- function(file) {
  obj <- XML::xmlTreeParse(file=file) %>%
    XML::xmlRoot(.)
  obj_list <- xmlSApply(obj, function(x) xmlSApply(x, xmlValue))
}
