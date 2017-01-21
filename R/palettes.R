#' Color palettes
#'
#' @description Color palettes from another time and place.
#'
#'
#' @param n The number of colors to return. The default is the length of the palette.
#'
#' @details This function returns the hex values of various colors.
#'
#' @seealso \code{\link[grDevices]{colors}}
#'
#' @return A character vector of hex values for colors.
#'
#' @examples
#' library(NineteenEightyR)
#' ggplot2::qplot(x=seq_along(sonny()), fill=sonny(), geom='tile', y='')


#' @rdname palettes
#' @export
sonny <- function(n=8) {
  # from http://www.colourlovers.com/palette/589234/crocketts_closet
  pal = c('#4C9385','#F2D1B3','#F2E77E','#DA7698','#E3A7C0','#4C9385','#F2D1B3','#F2E77E')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(pal, length.out=10)
}

#' @rdname palettes
#' @export
malibu <- function(n=8) {
  # from http://www.colourlovers.com/palette/140074/Malibu_circa_1984
  pal = c('#FF4E86','#FF9E44','#FDFD31','#85FD4E','#4ED5FF','#FF4E86','#FF9E44','#FDFD31')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(pal, length.out=10)
}

#' @rdname palettes
#' @export
hotpink <- function(n=5) {
  pal = grep('hotpink', grDevices::colors(), value=T)
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(base::as.hexmode(pal), length.out=n)
}

#' @rdname palettes
#' @export
youngturqs <- function(n=12) {
  pal = grep('turq', grDevices::colors(), value=T)
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(base::as.hexmode(pal), length.out=n)
}
