#' Color palettes
#'
#' @description Color palettes from another time and place.
#'
#'
#' @param n The number of colors to return. The default is the length of the palette.
#' @param alpha Transparency to be used in \link[scales]{col2hcl}.
#'
#' @details The functions returns the hex values of various colors inspired by
#'   Sonny Crockett, Malibu, Miami, the movie Cobra, and more.  Requires the
#'   scales package. Several are original from the colourlovers site via the
#'   package of the same name.
#'
#' @seealso \code{\link[grDevices]{colors}}
#'
#' @return A character vector of hex values for colors.  By default it will show
#'   the entire palette, otherwise the first 'n' or, it will repeat the palette
#'   to reach 'n' length.
#'   
#' @importFrom scales col2hcl
#'
#' @examples
#' library(NineteenEightyR); library(ggplot2)
#' qplot(x=seq_along(sonny()), y='') + 
#'   geom_tile(fill=sonny())
#' qplot(x=seq_along(malibu()), y='') + 
#'   geom_tile(fill=malibu())
#' qplot(x=seq_along(seventies_aint_done_yet()), y='') + 
#'   geom_tile(fill=seventies_aint_done_yet())
#' qplot(x=seq_along(youngturqs()), y='') + 
#'   geom_tile(fill=youngturqs())
#' qplot(x=seq_along(cobra()), y='') + 
#'   geom_tile(fill=cobra())



#' @rdname palettes
#' @export
sonny <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # from http://www.colourlovers.com/palette/589234/crocketts_closet
  # pal <- colourlovers::swatch(colourlovers::clpalette(589234))[[1]]
  pal <- c('#4C9385', '#F2D1B3', '#F2E77E', '#DA7698', '#E3A7C0')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(pal, length.out=n)
}

#' @rdname palettes
#' @export
malibu <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # from http://www.colourlovers.com/palette/140074/Malibu_circa_1984
  # pal <- colourlovers::swatch(colourlovers::clpalette(140074))[[1]]
  pal <- c('#FF4E86', '#FF9E44', '#FDFD31', '#85FD4E', '#4ED5FF') 
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(pal, length.out=n)
}

#' @rdname palettes
#' @export
hotpink <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  pal <- grep('hotpink', grDevices::colors(), value=T)
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
youngturqs <- function(n=12, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  pal <- grep('turq', grDevices::colors(), value=T)
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
sunset1 <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # pal <- colourlovers::swatch(colourlovers::clpalette(694737))[[1]]
  pal <- c('#ECD078FF', '#DA5B43FF', '#C12942FF', '#542437FF', '#53777AFF')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
sunset2 <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # pal <- colourlovers::swatch(colourlovers::clpalette(1032956))[[1]]
  pal <- c('#D99E4C', '#D65238', '#9E443F', '#422B3A', '#28212B')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
sunset3 <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # pal <- colourlovers::swatch(colourlovers::clpalette(3341748))[[1]]
  pal <- c('#390E40', '#701C34', '#A63228', '#C7662A', '#EBA42B')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
miami1 <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # pal <- colourlovers::swatch(colourlovers::clpalette(92095))[[1]]
  pal <- c('#69D2E7', '#A7DBD8', '#E0E4CC', '#F38630', '#FA6900')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
miami2 <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # pal <- colourlovers::swatch(colourlovers::clpalette(932683))[[1]]
  pal <- c('#3FB8AF', '#7FC7AF', '#DAD8A7', '#FF9E9D', '#FF3D7F')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
seventies_aint_done_yet <- function(n=5, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  # pal <- colourlovers::swatch(colourlovers::clpalette(699393))[[1]]
  pal <- c('#FEDF37', '#FCA811', '#D25117', '#8A4C19', '#573420')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}

#' @rdname palettes
#' @export
cobra <- function(n=6, alpha=1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  pal <- c('#ef342c', '#e85569', '#111013', '#cb302c', '#5c2c2d', '#3E509A')
  if (n > length(pal)) warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha=alpha), length.out=n)
}


#' @rdname palettes
#' @export
electronic_night <- function(n = 5, alpha = 1) {
  if(isFALSE(is.numeric(n)) | 
     isFALSE(is.numeric(alpha)) | 
     (alpha < 0 | alpha > 1))
    stop('Arguments must be numeric, and 0 <= alpha <= 1.')
  
  pal <- c('#cb54d6', '#3e45c4', '#21191a', '#362f78', '#57b4ae')
  if (n > length(pal))
    warning('n greater than number of colors.')
  rep(scales::col2hcl(pal, alpha = alpha), length.out = n)
}

