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


#' Generate a new brewlog database
#'
#' @return
#' @export
#'
#' @examples
#' @import tibble
newBrewLog_db <- function() {
  out <- data_frame(
    batch_id = numeric(0),
    brewDate = character(0),
    startTime = character(0),
    stopTime = character(0),
    recipe_URL = character(0),
    recipe_title = character(0),
    style = character(0),
    type = character(0),
    category = character(0),
    est_og = numeric(0),
    est_fg = numeric(0),
    vol_preB_target = numeric(0),
    vol_preB_measured = numeric(0),
    vol_first_runnings = numeric(0),
    vol_second_runnings = numeric(0),
    vol_postB_target = numeric(0),
    vol_postB_measured = numeric(0),
    vol_ferm_target = numeric(0),
    vol_ferm_measured = numeric(0),
    notes_recipe = character(0),
    units_vol = character(0),
    units_salt = character(0),
    units_fermentables = character(0),
    units_hops = character(0),
    h2o_caplus2 = numeric(0),
    h2o_mgplus2 = numeric(0),
    h2o_naplus = numeric(0),
    h2o_clminus = numeric(0),
    h2o_so4minus2 = numeric(0),
    h2o_hco3minus = numeric(0),
    salts_added_type1 = character(0),
    salts_added_amt1 = numeric(0),
    salts_added_type2 = character(0),
    salts_added_amt2 = numeric(0),
    salts_added_type3 = character(0),
    salts_added_amt3 = numeric(0),
    salts_added_type4 = character(0),
    salts_added_amt4 = numeric(0),
    salts_added_type5 = character(0),
    salts_added_amt5 = numeric(0),
    pH_target = numeric(0),
    pH_type = character(0),
    pH_amt = numeric(0),
    notes_h2o = character(0)
  )
  return(out)
}
