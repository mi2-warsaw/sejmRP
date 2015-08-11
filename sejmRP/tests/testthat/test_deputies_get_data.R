test_that("result of function", {
  expect_equal(class(deputies_get_data()), "data.frame")
})

test_that("columns of table", {
  expect_equal(ncol(deputies_get_data()), 2)
})


test_that("rows of table", {
  expect_more_than(nrow(deputies_get_data()), 0)
})