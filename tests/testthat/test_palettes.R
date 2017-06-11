context('test color palettes')

test_that("palette returns a character string", {
  expect_type(sonny(5), 'character')
})


test_that("example returns a ggplot", {
  library(ggplot2)
  expect_s3_class(qplot(x=seq_along(malibu()), y='') + geom_tile(fill=malibu()), 'ggplot')
})
