test_that("result of function", {
  expect_equal(class(deputies_get_data("active")), "data.frame")
})

test_that("result of function", {
  expect_equal(class(deputies_get_data("inactive")), "data.frame")
})

test_that("columns of table", {
  expect_equal(ncol(deputies_get_data("active")), 2)
})

test_that("columns of table", {
  expect_equal(ncol(deputies_get_data("inactive")), 2)
})

test_that("rows of table", {
  expect_more_than(nrow(deputies_get_data("active")), 0)
})

test_that("rows of table", {
  expect_more_than(nrow(deputies_get_data("inactive")), 0)
})