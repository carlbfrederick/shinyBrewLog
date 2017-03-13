#' Title
#'
#' taken from vignette at http://www.omegahat.net/RSXML/gettingStarted.html
#'
#' @param file
#'
#' @return
#' @export
#'
#' @importFrom XML xmlTreeParse
#' @importFrom XML xmlRoot
#' @importFrom magrittr `%>%`
#' @importFrom tibble data_frame
importBXML <- function(file) {
  obj <- XML::xmlParse(file=file) %>%
    XML::xmlToList(.)

  obj_lists <- sapply(obj$RECIPE, is.list)

  main <- obj$RECIPE[!obj_lists]
  style <- obj$RECIPE$STYLE

  out <- vector("list", 0)

  out$TITLE <- main$NAME
  out$STYLE <- style$NAME
  out$TYPE <- style$TYPE
  out$CAT  <- paste(style$CATEGORY, " (", style$STYLE_GUIDE, " ", style$CATEGORY_NUMBER, style$STYLE_LETTER, ")", sep="")
  out$BATCH_SIZE <- liters2gal(main$BATCH_SIZE)
  out$BOIL_SIZE <- liters2gal(main$BOIL_SIZE)
  out$EST_OG <- strip_sg(main$EST_OG)
  out$EST_FG <- strip_sg(main$EST_FG)

  return(out)
}

#' Title
#'
#' @param liters
#'
#' @return
liters2gal <- function(liters) {
  liters <- as.numeric(liters)
  gal <- round(0.264172 * liters, 1)
  return(gal)
}


#' Title
#'
#' @param gravity
#'
#' @return
#'
#' @importFrom stringr str_extract
strip_sg <- function(gravity) {
  numGrav <- as.numeric(stringr::str_extract(gravity, "^[0-9.]*"))
  return(numGrav)
}
