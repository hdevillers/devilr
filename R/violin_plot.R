#' Violin plot
#' @description
#' Performs a violin plot from a list of values.
#'
#' @param x a list of values or a data.frame.
#' @param y.lim y axis limits.
#' @param y.lab y axis label.
#' @param mode plotting mode: only `left`, only `right` or `both`.
#' @param cols color generator or vector of colors.
#' @param add.pt boolean, add points over distributions.
#' @param cex.pt points size factor.
#' @param add.cnt boolean, add counts.
#' @param new.plot boolean, initialize a new plot.
#' @param spacer spacer between each distribution.
#' @param smooth.lvl smooth level.
#'
#' @return nothing
#' @export
#'
#' @examples
#' # plot normal distribution
#' dt <- list(
#'   grp1 = rnorm(100, 0, 1),
#'   grp2 = rnorm(200, 1, 3)
#' )
#' violin_plot(dt)
violin_plot <- function(x, y.lim = NULL, y.lab = "", mode = "both", cols=my_rainbow,
                        add.pt = FALSE, cex.pt = 0.8, add.cnt = FALSE, new.plot = TRUE,
                        spacer = 0.05, smooth.lvl = 1) {
  if( !is.list(x) ) {
    stop("This function only accepte a list of values as input.")
  }
  if( smooth.lvl <= 0 ) {
    stop("Smooth level (smooth) must be greater than 0.")
  }
  n <- length(x)
  x.at <- NULL
  x.delta <- NULL
  if(n == 1) {
    x.at = 0.5
    x.delta = 0.5
  } else {
    x.at <- seq(0, 1, len = n)
    x.delta <- (x.at[2] - x.at[1])/2 * (1-spacer)
  }
  x.min <- min(x.at - x.delta)
  x.max <- max(x.at + x.delta)
  x.lab <- names(x)
  if( is.null(x.lab) ) {
    x.lab <- paste(rep("X", n), 1:n, sep="")
  }
  y.min <- NULL
  y.max <- NULL
  if( is.null(y.lim) ) {
    y.min <- min(sapply(x, min, na.rm = TRUE))
    y.max <- max(sapply(x, max, na.rm = TRUE))
    if( y.min == -Inf || x.max == Inf ) {
      stop("Input(s) must have finite values.")
    }
  } else {
    y.min <- y.lim[1]
    y.max <- y.lim[2]
  }

  # Manage colors
  col.bg <- NULL
  col.bd <- NULL
  col.pt <- NULL
  if( methods::is(cols, "function") ) {
    col.bd <- cols(n)
  } else {
    col.bd = cols
  }
  col.bg <- color_lighten(col.bd, 0.5)
  col.pt <- color_alpha(col.bd, 0.3)

  # Empty plot
  if( new.plot ) {
    graphics::plot(0, 0, type = "n", axes = FALSE, ylab = y.lab, xlab = "",
         xlim = c(x.min, x.max), ylim = c(y.min, y.max))
    graphics::box()
    graphics::axis(1, at = x.at, labels = x.lab, las = 2)
    graphics::axis(2)
    graphics::abline(v = x.at, lty = 3, col = grDevices::grey(0.5, 0.5))
  }
  tmp <- mapply(add_violin,
                x, x.at, col.bg = col.bg, col.bd = col.bd, col.pt = col.pt,
                MoreArgs = list(width = x.delta, mode = mode, cex.pt = cex.pt, add.pt = add.pt, smooth.lvl = smooth.lvl)
                )
  if( add.cnt && new.plot ) {
    cnt <- sapply(x, length)
    graphics::mtext(paste(rep("n=", length(cnt)), cnt, sep=""), side=3, line = 1, at = x.at)
  }
}

#' Add a violin on a current plot
#' @description
#' Internal function to add violin view on a plot.
#'
#' @noRd
add_violin <- function(val, at, width, mode, add.pt, col.bg, col.bd, col.pt, cex.pt, smooth.lvl) {
  den <- stats::density(val , from = min(val), to = max(val), na.rm = TRUE, adjust = smooth.lvl)
  den.x <- den$y / max(den$y) * width
  den.y <- den$x
  den.n <- length(den.y)
  ft.x <- NULL
  vio.y <- NULL
  vio.x <- NULL
  app.x <- stats::approx(den$x, den.x, val)$y
  if( mode == "both" ) {
    vio.x <- c(at - den.x, rev(at + den.x))
    vio.y <- c(den.y, rev(den.y))
    ft.x <- cbind(at-app.x, at+app.x)
    graphics::polygon(vio.x, vio.y, border = col.bd, col = col.bg)
  } else if( mode == "left" ) {
    vio.x <- c(at, at - den.x, at)
    vio.y <- c(den.y[1], den.y, den.y[den.n])
    ft.x <- cbind(at-app.x, rep(at, length(app.x)))
    graphics::polygon(vio.x, vio.y, border = col.bg, col = col.bg)
    graphics::lines(vio.x, vio.y, col = col.bd)
  } else if (mode == "right" ) {
    vio.x <- c(at, rev(at + den.x), at)
    vio.y <- c(den.y[den.n], rev(den.y), den.y[1])
    ft.x <- cbind(rep(at, length(app.x)), at+app.x)
    graphics::polygon(vio.x, vio.y, border = col.bg, col = col.bg)
    graphics::lines(vio.x, vio.y, col = col.bd)
  } else {
    stop(paste("Unsupported plot mode (", mode, ").", sep=""))
  }

  if( add.pt ) {
    x.pt <- apply(ft.x, 1, function(x) return(stats::runif(1, x[1], x[2])))
    graphics::points(x.pt, val, pch = 19, col = col.pt, cex = cex.pt)
  }
}
