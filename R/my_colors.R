#' Lighten a vector of colors
#'
#' @param col a color or a vector of colors (text or hexadecimal).
#' @param fac a float number between 0 and 1 that indicate the lightening intensity.
#'
#' @return a vector of lightened color(s).
#' @export
#'
#' @examples
#' # From a text color
#' color_lighten("red", 0.2)
#'
#' # From a vector of hexadecimal colors
#' color_lighten(rainbow(10), 0.5)
color_lighten <- function(col, fac = 0.1) {
  if( fac < 0 || fac > 1 ) {
    stop("Lighten factor (fac) must be between 0 and 1.")
  }
  dec <- grDevices::col2rgb(col)
  out <- apply(dec, 2, function(val) {
    re <- min(255, val[1] * (1-fac) + 255 * fac)/255
    gr <- min(255, val[2] * (1-fac) + 255 * fac)/255
    bl <- min(255, val[3] * (1-fac) + 255 * fac)/255
    al <- 1
    if( length(val) == 4 ) {
      al <- val[4]/255
    }
    return(grDevices::rgb(re, gr, bl, alpha = al))
  })
  return(out)
}

#' Darken a vector of colors
#'
#' @param col a color or a vector of colors (text or hexadecimal).
#' @param fac a float number between 0 and 1 that indicate the darkening intensity.
#'
#' @return a vector of darkened color(s).
#' @export
#'
#' @examples
#' # From a text color
#' color_darken("red", 0.2)
#'
#' # From a vector of hexadecimal colors
#' color_darken(rainbow(10), 0.5)
color_darken <- function(col, fac = 0.1) {
  if( fac < 0 || fac > 1 ) {
    stop("Darken factor (fac) must be between 0 and 1.")
  }
  dec <- grDevices::col2rgb(col)
  out <- apply(dec, 2, function(val) {
    re <- max(0, val[1] * (1-fac))/255
    gr <- max(0, val[2] * (1-fac))/255
    bl <- max(0, val[3] * (1-fac))/255
    al <- 1
    if( length(val) == 4 ) {
      al <- val[4]/255
    }
    return(grDevices::rgb(re, gr, bl, alpha = al))
  })
  return(out)
}

#' Reset the alpha level of a vector of colors
#'
#' @param col a color or a vector of colors (text or hexadecimal).
#' @param alpha alpha value, float number between 0 and 1.
#'
#' @return a vector of color(s).
#' @export
#'
#' @examples
#' # From a text color
#' color_alpha("red", 0.2)
#'
#' # From a vector of hexadecimal colors
#' color_alpha(rainbow(10), 0.5)
color_alpha <- function(col, alpha = 1.0) {
  if( alpha < 0 || alpha > 1 ) {
    stop("Alpha factor (alpha) must be between 0 and 1.")
  }
  dec <- grDevices::col2rgb(col)
  out <- apply(dec, 2, function(val) {
    re <- val[1]/255
    gr <- val[2]/255
    bl <- val[3]/255
    return(grDevices::rgb(re, gr, bl, alpha = alpha))
  })
  return(out)
}

#' Mix a vector of colors with another color
#'
#' @param col1 a color or a vector of colors (text or hexadecimal).
#' @param col2 a unique color to be mixed with col1.
#' @param p1 proportion of col1, float number between 0 and 1.
#'
#' @return a vector of mixed color(s).
#' @export
#'
#' @examples
#' # From a text color
#' color_mixer("red", "blue", 0.2)
#'
#' # From a vector of hexadecimal colors
#' color_alpha(rainbow(10), 0.5)
color_mixer <- function(col1, col2, p1 = 0.5) {
  if(p1<0 || p1>1) {
    stop("The proportion of color 1 must be between 0 and 1.")
  }
  if( length(col2) > 1 ) {
    stop("The destination color (col2) must be unique.")
  }
  col2.dec <- grDevices::col2rgb(col2)
  col1.dec <- grDevices::col2rgb(col1)
  out <- apply(col1.dec, 2, function(val) {
    re <- (val[1] * p1 + (1-p1) * col2.dec[1,1])/255
    gr <- (val[2] * p1 + (1-p1) * col2.dec[2,1])/255
    bl <- (val[3] * p1 + (1-p1) * col2.dec[3,1])/255
    al <- 1
    if( length(val) == 4 ) {
      al <- val[4]/255
    }
    return(grDevices::rgb(re, gr, bl, alpha = al))
  })
  return(out)
}

#' My red
#' @export
my.red <- "#C5001A"

#' My orange
#' @export
my.orange <- "#FB6542"

#' My yellow
#' @export
my.yellow <- "#FFBB00"

#' My green
#' @export
my.green <- "#6AB187"

#' My teal green
#' @export
my.tealgreen <- "#20948B"

#' My teal blue
#' @export
my.tealblue <- "#1995AD"

#' My blue
#' @export
my.blue <- "#375E97"

#' My purple
#' @export
my.purple <- "#735DA5"

#' My pink
#' @export
my.pink <- "#FA6775"

#' My rainbow color ramp
#'
#' @param n number of required colors.
#' @param alpha alpha factor.
#'
#' @return a vector of colors.
#' @export
#'
#' @examples
#' # Get 10 colors
#' my_rainbow(10)
my_rainbow <- function(n, alpha = 1) {
  if( alpha < 0 || alpha > 1 ) {
    stop("Alpha factor (alpha) must be between 0 and 1.")
  }
  crp <- grDevices::colorRampPalette(c(
    my.red, my.orange, my.yellow,
    my.green, my.tealblue, my.blue,
    my.purple
  ))
  cols <- crp(n)
  if( alpha != 1 ) {
    cols <- color_alpha(cols, alpha)
  }
  return(cols)
}


