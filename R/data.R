#' Annual Accumulated Growing Degree Days (°C) from PRISM
#'
#' @description A `raster` containing annual accumulated growing degree days (°C) with a base temperature of 10°C and maximum temperature of 30°C.
#' GDD was calculated from tmin and max 30 seconds (~1 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @format An object of class `RasterLayer`.
#' @source \url{https://www.worldclim.org/data/worldclim21.html}
"PRISM_annual_gdd"

#' Growing Season Accumulated Growing Degree Days (°C) from PRISM
#'
#' @description A `raster` containing accumulated growing degree days (°C) for the growing season (May through September) with a base temperature of 10°C and maximum temperature of 30°C.
#' GDD was calculated from tmin and max 30 seconds (~1 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @format An object of class `RasterLayer`.
#' @source \url{http://www.prism.oregonstate.edu/}
"PRISM_gs_gdd"

#' Four Corners County Shapefiles
#'
#' @description An `sf` `data.frame` containing county polygons for Arizona, Colorado, New Mexico, and Utah.
#'
#' @format An object of class `sfc_POLYGON`.
#' @source \url{https://github.com/adeckmyn/maps}
"SWUS_counties"

#' Radiocarbon Data for Four Corners
#'
#' @description A `data.frame` containing radiocarbon data for the Four Corners. `p3k14c` data was combined with `cropDiffusionR`.
#' @format An object of class `tbl_df` with the following variables:
#' \describe{
#' \item{\code{LabID}}{Lab ID of each radiocarbon date}
#' \item{\code{SiteID}}{ID of the site from which the sample has been recovered}
#' \item{\code{SiteName}}{Name of the site from which the sample has been recovered}
#' \item{\code{county_st}}{County and state of the site from which the sample has been recovered}
#' \item{\code{NAME}}{County of the site from which the sample has been recovered}
#' \item{\code{REGION}}{State of the site from which the sample has been recovered}
#' \item{\code{Taxa}}{Material of the dated sample}
#' \item{\code{Age}}{Radiocarbon age in 14C years BP}
#' \item{\code{Error}}{Radiocarbon age error}
#' \item{\code{calib}}{Calibration curve to be used to calibrate the Age}
#'}
#' @source \url{https://github.com/people3k/p3k14c}
#' @source \url{https://github.com/Archaeo-Programmer/cropDiffusionR/blob/main/vignettes/tables}
"sw_rc"

#' Summed Probability Distributions for Four Corners Counties
#'
#' @description A `tibble` containing SPDs for counties in Arizona, Colorado, New Mexico, and Utah. Also, contains the number of samples and original data used to make the SPD.
#'
#' @format An object of class `tbl_df`.
#' \describe{
#' \item{\code{NAME}}{County of the site from which the sample has been recovered}
#' \item{\code{REGION}}{State of the site from which the sample has been recovered}
#' \item{\code{n}}{Sample size of radiocarbon dates for each county}
#' \item{\code{data}}{Radiocarbon samples in each county as a nested `tibble`}
#' \item{\code{spds}}{SPDs for each county as an object of class `CalGrid`}
#' \item{\code{county_st}}{County and state of the site from which the sample has been recovered}
#'}
"SPD_county"
