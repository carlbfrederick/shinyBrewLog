#' Pre-Boil Efficiency
#'
#' @param sg_pb_target Predicted Original Gravity (1.XXX)
#' @param eff_ass Assumed Efficiency
#' @param vol_pb_target Target Pre-Boil Volume
#' @param sg_pb_measured Measured Pre-Boil Specific Gravity
#' @param vol_pb_measured Measured Pre-Boil Volume of Wort Collected
#' @param lbs_grain Total pounds of grain
#'
#' @return
#' @export
#'
#' @examples
eff_pb <- function(sg_pb_target, eff_ass, vol_pb_target, sg_pb_measured, vol_pb_measured, lbs_grain) {
  gp_pred <- (1000*(sg_pb_target - 1)) * vol_pb_target
  gp_actual <- (1000*(sg_pb_measured - 1)) * vol_pb_measured
  eff_pb <- gp_actual / (gp_pred / eff_ass)
  return(list(ppg = gp_actual / lbs_grain,
              eff_pb = eff_pb))
}
