context('test color palettes')


# test sonny --------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(sonny(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(sonny('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(sonny(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(sonny(25))
})


# test malibu -------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(malibu(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(malibu('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(malibu(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(malibu(25))
})

# test hotpink ------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(hotpink(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(hotpink('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(hotpink(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(hotpink(25))
})

# test turq ---------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(youngturqs(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(youngturqs('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(youngturqs(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(youngturqs(25))
})

# test sunset -------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(sunset1(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(sunset1('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(sunset1(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(sunset1(25))
})

test_that("palette returns a character string", {
  expect_type(sunset2(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(sunset2('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(sunset2(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(sunset2(25))
})

test_that("palette returns a character string", {
  expect_type(sunset3(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(sunset3('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(sunset3(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(sunset3(25))
})

# test miami --------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(miami1(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(miami1('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(miami1(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(miami1(25))
})


test_that("palette returns a character string", {
  expect_type(miami2(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(miami2('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(miami2(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(miami2(25))
})


# test 70s ----------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(seventies_aint_done_yet(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(seventies_aint_done_yet('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(seventies_aint_done_yet(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(seventies_aint_done_yet(25))
})


# test cobra --------------------------------------------------------------


test_that("palette returns a character string", {
  expect_type(cobra(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(cobra('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(cobra(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(cobra(25))
})


# test electronic night ---------------------------------------------------

test_that("palette returns a character string", {
  expect_type(electronic_night(5), 'character')
})

test_that("palette errors with incorrect input", {
  expect_error(electronic_night('a'))
})

test_that("palette errors with incorrect input", {
  expect_error(electronic_night(5, -1))
})

test_that("palette warns with big n", {
  expect_warning(electronic_night(25))
})



