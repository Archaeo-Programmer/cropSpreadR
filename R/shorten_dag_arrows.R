#' @name shorten_dag_arrows
#' @title Adjust Length of Dag Arrows
#'
#' @description `shorten_dag_arrows()` scales down dag arrows from both ends according to a proportion.
#'
#' @param tidy_dag A `tidy_dagitty` object containing a tidy dag.
#' @return A tidy_dagitty with the adjusted dag arrows.
#' @importFrom magrittr `%<>%` `%>%`
#' @export
shorten_dag_arrows <- function(tidy_dag, proportion) {
  # Update underlying ggdag object
  tidy_dag$data <- dplyr::mutate(
    tidy_dag$data,
    xend = (1 - proportion / 2) * (xend - x) + x,
    yend = (1 - proportion / 2) * (yend - y) + y,
    xstart = (1 - proportion / 2) * (x - xend) + xend,
    ystart = (1 - proportion / 2) * (y - yend) + yend
  )
  return(tidy_dag)
}
