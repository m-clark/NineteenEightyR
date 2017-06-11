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
#' @return A character vector of hex values for colors.  By default it will show the entire palette, otherwise the first 'n' or, it will repeat the palette to reach 'n' length.
#'
#' @importFrom scales col2hcl
#'
#' @examples
#' library(NineteenEightyR); library(ggplot2)
#' qplot(x=seq_along(sonny()), y='') + geom_tile(fill=sonny())
#' qplot(x=seq_along(malibu()), y='') + geom_tile(fill=malibu())
#' qplot(x=seq_along(seventies_aint_done_yet()), y='') + geom_tile(fill=seventies_aint_done_yet())
#' qplot(x=seq_along(youngturqs()), y='') + geom_tile(fill=youngturqs())
#' qplot(x=seq_along(cobra()), y='') + geom_tile(fill=cobra())



#' @rdname palettes
#' @export
sonny <- function(n=5) {
  # from http://www.colourlovers.com/palette/589234/crocketts_closet
  pal = colourlovers::swatch(colourlovers::clpalette(589234))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(pal, length.out=n)
}

#' @rdname palettes
#' @export
malibu <- function(n=5) {
  # from http://www.colourlovers.com/palette/140074/Malibu_circa_1984
  pal = colourlovers::swatch(colourlovers::clpalette(140074))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(pal, length.out=n)
}

#' @rdname palettes
#' @export
hotpink <- function(n=5) {
  pal = grep('hotpink', grDevices::colors(), value=T)
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
youngturqs <- function(n=12) {
  pal = grep('turq', grDevices::colors(), value=T)
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
sunset1 <- function(n=5) {
  pal = colourlovers::swatch(colourlovers::clpalette(694737))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
sunset2 <- function(n=5) {
  pal = colourlovers::swatch(colourlovers::clpalette(1032956))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
sunset3 <- function(n=5) {
  pal = colourlovers::swatch(colourlovers::clpalette(3341748))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
miami1 <- function(n=5) {
  pal = colourlovers::swatch(colourlovers::clpalette(92095))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
miami2 <- function(n=5) {
  pal = colourlovers::swatch(colourlovers::clpalette(932683))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
seventies_aint_done_yet <- function(n=5) {
  pal = colourlovers::swatch(colourlovers::clpalette(699393))[[1]]
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

#' @rdname palettes
#' @export
cobra <- function(n=6) {
  pal = c('#ef342c', '#e85569', '#111013', '#cb302c', '#5c2c2d', '#3E509A')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}


#' @rdname palettes
#' @export
electronic_night <- function(n=5) {
  pal = c('#cb54d6', '#3e45c4', '#21191a', '#362f78', '#57b4ae')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal), length.out=n)
}

