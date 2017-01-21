#' 198R
#'
#' @description Just a fun onLoad display.
#'
#'
#'
display_198R = function(){
  graphics::plot(c(0, 490), c(0, 549), type = "n", bty='n', xaxt='n', xlab='', yaxt='n', ylab='', bg='black')
  graphics::rasterImage(rpng, 0, 0, 490, 549)  # rpng created with png::readPng and saved to R/sysdata.rda via devtools::use_data(rpng, internal=T)
}
