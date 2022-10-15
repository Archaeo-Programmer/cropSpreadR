#' @name calc_ckde_dates
#' @title Calculate Composite Kernel Density Estimates of Radiocarbon Dates
#'
#' @description `calc_ckde_dates()` calibrates the uncalibrated radiocarbon dates, then samples the dates with simulation and bootstrap,
#' then returns the mean density estimate, the high and low values for the quantile interval, and a BC/AD date column as a dataframe.
#'
#' @param data A `data.frame` containing radiocarbon dates.
#' @param interval A `numeric` value for the quantile interval. Default is 0.95.
#' @param timeRange_max A `numeric` value for the maximum time range in BP.
#' @param timeRange_min A `numeric` value for the minimum time range in BP.
#' @return A tibble that has the mean density estimate, the high and low values for the quantile interval, and a BC/AD date.
#' @importFrom magrittr `%<>%` `%>%`
#' @export
calc_ckde_dates <-
  function(data,
           interval = 0.95,
           timeRange_max,
           timeRange_min) {
    cptcal <- rcarbon::calibrate(
      x = data$Age,
      errors = data$Error,
      calCurves = data$calib,
      verbose = FALSE
    )
    sampledDates <-
      rcarbon::sampleDates(cptcal, nsim = 1000, boot = TRUE)
    sw_ckde <-
      rcarbon::ckde(sampledDates,
                    timeRange = c(timeRange_max, timeRange_min),
                    bw = 50)

    avg = apply(sw_ckde$res.matrix, 1, mean, na.rm = TRUE)
    lo = apply(sw_ckde$res.matrix,
               1,
               quantile,
               prob = (1 - interval) / 2,
               na.rm = TRUE)
    hi = apply(
      sw_ckde$res.matrix,
      1,
      quantile,
      prob = interval + (1 - interval) / 2,
      na.rm = TRUE
    )

    data.frame(Date = timeRange_max:timeRange_min, avg, lo, hi) %>%
      dplyr::mutate(Date = rcarbon::BPtoBCAD(Date)) %>%
      tidyr::drop_na(lo, hi)
  }
