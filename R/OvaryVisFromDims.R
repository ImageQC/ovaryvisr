#' a simple heuristic to determine ovary visualisation
#'
#'
#' @param d1 Numeric dimension D1 in mm
#' @param d2 Numeric dimension D3 in mm
#' @param d3 Numeric dimension D3 in mm
#'
#' @return string not visualised or visualised depending upon inputs
#'
#' @examples OvaryVisFromDims(20, 12,11)
#' @export
OvaryVisFromDims <- function(d1, d2, d3)
{
  rc <- "not visualised"
  if ((d1 > 14) && (d3 > 0))
  {
    ratio <- d2 / d3;
    if ((ratio > 0.5) && (ratio < 1.5))
    {
      vol <- ((d1 * d2 * d3) * 0.523) / 1000;
      if ((vol > 0.8 ) && (vol < 5.0))
        rc <- "visualised"
    }
  }
  return (rc)
}
