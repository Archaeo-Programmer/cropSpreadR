#' Annual Accumulated Growing Degree Days from PRISM.
#'
#' A `raster` containing annual accumulated growing degree days with a base temperature of 10°C and maximum temperature of 30°C.
#' GDD was calculated from tmin and max 30 seconds (~1 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @format An object of class `RasterLayer`.
#' @source \url{https://www.worldclim.org/data/worldclim21.html}
"PRISM_annual_gdd"

#' Growing Season Accumulated Growing Degree Days from PRISM.
#'
#' A `raster` containing accumulated growing degree days for the growing season (May through September) with a base temperature of 10°C and maximum temperature of 30°C.
#' GDD was calculated from tmin and max 30 seconds (~1 \ifelse{html}{\out{km<sup>2</sup>}}{\eqn{km^2}}).
#'
#' @format An object of class `RasterLayer`.
#' @source \url{https://www.worldclim.org/data/worldclim21.html}
"PRISM_gs_gdd"
