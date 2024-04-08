#' Sliding window function
#'
#' @param x a vector of values
#' @param sw.length length of the window
#' @param sw.step length of the steps between windows
#' @param keep.last if length of `x` is not a multiple of `sw.step` then add a last frame with a conserved length but with a shorter step
#' @param mode computation mode of the sliding window algorithm
#' @param na.act how to treat `NA` values
#' @param only.values if `TRUE` only the computed values are returned
#' @param index.adj index adjustment
#'
#' @return an array or a vector of values
#' @export
#' @useDynLib devilr, .registration=TRUE
#'
#' @examples
#' # Create an noisy sine wave signal
#' x <- seq(-7, 7, len = 20000)
#' y <- 4 * sin(x) + rnorm(length(x), 0, 0.5)
#' sw.mean <- sliding_window(y, 100, 50, mode = "mean")
#'
#' # Draw it
#' plot(x, y, type="l")
#' lines(x[sw.mean[,1]], sw.mean[,2], col = "red")
#'
#' # Add NAs and treat the data
#' na.id <- sample(1:length(y), 100, replace = FALSE)
#' y[na.id] <- NA
#' sw.na <- sliding_window(y, 100, 50, mode = "mean", na.act = "median")
#'
#' # Try sum mode
#' sw.sum <- sliding_window(y, 100, 50, mode = "sum", na.act = "median")
sliding_window <- function(x = NULL, sw.length, sw.step, keep.last = TRUE,
                            mode = "mean", na.act = "mean", only.values = FALSE,
                            index.adj = 0.5) {
  # Check mode value
  mode <- tolower(mode)
  if( !(mode %in% c("mean", "median", "min", "max", "sum")) ) {
    stop(paste("Unsupported mode (", mode, ")", sep = ""))
  }

  # Check NA action value
  na.act <- tolower(na.act)
  if( !(na.act %in% c("mean", "median", "random")) )

    # Check sw.length and sw.step values
    if( as.integer(sw.length) != as.double(sw.length) ) {
      stop("Sliding window length (sw.length) must be an integer.")
    }
  if( sw.length <= 1 ) {
    stop("Sliding window length (sw.length) must be at least 2.")
  }
  if( as.integer(sw.step) != as.double(sw.step) ) {
    stop("Sliding window length (sw.length) must be an integer.")
  }
  if( sw.step <= 0 ) {
    stop("Sliding window length (sw.length) must be at least 1.")
  }

  # Check x
  if( length(x) == 0 ) {
    stop("You must provide a non-null vector of values (x).")
  }
  if( !is.numeric(x) ) {
    stop("The vector of values (x) must be numeric.")
  }

  # Check x.adj
  if( index.adj < 0 || index.adj > 1 ) {
    stop("The coordinate adjustment parameter (x.adj) must be between 0 and 1.")
  }

  # Manage NA
  na.id <- which(is.na(x))
  na.cnt <- length(na.id)
  if( na.cnt > 0 ) {
    x.nona <- stats::na.omit(x)
    if( length(x.nona) == 0 ) {
      stop("The vector of values (x) only contains NA.")
    }
    na.replace <- NULL
    if( na.act == "mean" ) {
      na.replace <- mean(x.nona)
    } else if( na.act == "median" ) {
      na.replace <- stats::median(x.nona)
    } else {
      na.replace <- sample(x.nona, na.cnt, replace = FALSE)
    }
    x[na.id] <- na.replace
  }

  # Compute step coordinates
  x.n <- length(x)
  split.n <- floor( (x.n - sw.length) / sw.step ) # Number of splits
  step.from <- seq(0, split.n * sw.step, sw.step)
  if( keep.last ) {
    if( split.n * sw.step + sw.length < x.n) {
      step.from <- c(step.from, x.n - sw.length)
      split.n <- split.n + 1
    }
  }

  # Run the .C call to sliding window algorithms
  sw.values <- rep(0, split.n+1)
  if( mode == "mean" ) {
    out <- .C('sliding_window_mean',
              as.double(x),
              as.double(sw.values),
              as.integer(step.from),
              as.integer(split.n+1),
              as.integer(sw.length))
    sw.values <- out[[2]]
  } else if( mode == "sum" ) {
    out <- .C('sliding_window_sum',
              as.double(x),
              as.double(sw.values),
              as.integer(step.from),
              as.integer(split.n+1),
              as.integer(sw.length))
    sw.values <- out[[2]]
  } else if( mode == "max" ) {
    out <- .C('sliding_window_max',
              as.double(x),
              as.double(sw.values),
              as.integer(step.from),
              as.integer(split.n+1),
              as.integer(sw.length))
    sw.values <- out[[2]]
  } else if( mode == "min" ) {
    out <- .C('sliding_window_min',
              as.double(x),
              as.double(sw.values),
              as.integer(step.from),
              as.integer(split.n+1),
              as.integer(sw.length))
    sw.values <- out[[2]]
  } else if( mode == "median" ) {
    out <- .C('sliding_window_median',
              as.double(x),
              as.double(sw.values),
              as.integer(step.from),
              as.integer(split.n+1),
              as.integer(sw.length))
    sw.values <- out[[2]]
  }

  # Return results
  if( only.values ) {
    return(sw.values)
  } else {
    x.ind <- floor(step.from + index.adj * sw.length) + 1
    return(cbind(
      sw.index = x.ind,
      sw.values = sw.values
    ))
  }
}
