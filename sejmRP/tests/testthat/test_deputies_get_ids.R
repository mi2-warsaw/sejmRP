test_that("result of function", {
  expect_equal(class(deputies_get_ids("sejmrp","sejmrp","x56jK99ZwQp","192.168.137.38",TRUE)),
    "character")
})

test_that("names", {
  expect_named(deputies_get_ids("sejmrp","sejmrp","x56jK99ZwQp","192.168.137.38",TRUE))
})


test_that("length", {
  expect_more_than(length(deputies_get_ids("sejmrp","sejmrp","x56jK99ZwQp","192.168.137.38",TRUE)), 0)
})