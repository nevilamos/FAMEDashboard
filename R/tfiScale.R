
#' tfiScale
#' Makes five colour scale for TFI "grey80","grey60","blue4","deepskyblue2","orange1"
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
tfiScale <- function(...) {
  ggplot2:::manual_scale("fill",
                         values = setNames(
                           c(
                             "grey80",
                             "grey60",
                             "blue4",
                             "deepskyblue2",
                             "orange1"
                           ),
                           tfiLevels
                         )
  )
}