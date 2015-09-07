test_that("result of function", {
  expect_equal(class(deputies_get_ids("sejmrp", "sejmrp", password, "192.168.137.38", .Platform$OS.type == "windows")),
    "character")
})

test_that("names", {
  expect_named(deputies_get_ids("sejmrp", "sejmrp", password, "192.168.137.38", .Platform$OS.type == "windows"))
})


test_that("length", {
  expect_more_than(length(deputies_get_ids("sejmrp", "sejmrp", password, "192.168.137.38", .Platform$OS.type == "windows")), 0)
})