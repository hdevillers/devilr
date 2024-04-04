#' Sliding window function
#'
#' @param x a vector of values
#' @param sw_len length of the window
#' @param sw_steps length of the steps between windows
#'
#' @return a vector of values
#' @export
#' @useDynLib devilr, .registration=TRUE
#'
#' @examples
#' # On a random vector
#' x <- rnorm(1010)
#' x.sw <- sliding_window(x, 100, 50)
sliding_window <- function(x, sw_len, sw_steps) {
  x_length <- length(x)
  n_counts <- floor((x_length - sw_len + 1) / sw_steps)
  counts <- rep(0, n_counts)

  out <- .C('sliding_window',
            as.double(x),
            as.double(counts),
            as.integer(n_counts),
            as.integer(sw_len),
            as.integer(sw_steps))
  return(out[[2]])
}
