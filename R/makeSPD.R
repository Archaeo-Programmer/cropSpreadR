#' @name makeSPD
#' @title Calibrate Dates and Calculate SPDs
#'
#' @description `makeSPD()` calibrates radiocarbon dates and calculates a summed probability distributions (SPD) of radiocarbon dates.
#'
#' @param data A data.frame containing radiocarbon dates.
#' @return A tibble that has a `CalGrid` class object containing the summed probability associated to each calendar year between timeRange[1] and timeRange[2].
#' @importFrom magrittr `%<>%` `%>%`
#' @export
makeSPD <- function(data) {
  cptcal <- rcarbon::calibrate(
    x = data$Age,
    errors = data$Error,
    calCurves = data$calib,
    verbose = FALSE
  )
  cptspd <- rcarbon::spd(
    x = cptcal,
    timeRange = c(8000, 0),
    spdnormalised = FALSE,
    # runm = 200,
    verbose = FALSE
  )
  return(cptspd$grid)
}
