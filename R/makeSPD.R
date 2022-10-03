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
