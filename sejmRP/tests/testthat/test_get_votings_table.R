test_that("result of function", {
  expect_equal(class(get_votings_table(host="192.168.137.38")), "data.frame")
})

test_that("columns of table", {
  expect_equal(ncol(get_votings_table(host="192.168.137.38")), 6)
})

test_that("rows of table", {
  expect_more_than(nrow(get_votings_table(host="192.168.137.38")), 0)
})