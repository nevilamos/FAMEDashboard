#' Calculate Geometric mean
#'
#' @details Calculates geometric mean of a vector of positive numeric values
#' @param x numeric vector
#'
#' @return numeric or NA if values are not all > 0
#' @export
geoMean <- function(x) {
  if (any(is.na(x))) {
    y <- NA
    warning("vector contains NA values")
  } else if (any(x <= 0)) {
    y <- NA
    warning("Not all x are positive values")
  } else {
    y <- prod(x)^(1 / length(x))
  }
  return(y)
}