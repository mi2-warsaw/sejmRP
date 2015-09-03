test_that("result of function", {
  expect_equal(class(get_filtered_votes(host = "192.168.137.38")), "data.frame")
})

test_that("result of function", {
  expect_equal(class(get_filtered_votes(host = "192.168.137.38", clubs = c("PO","PiS"),
    dates = c("2014-01-01","2014-12-31"), topics = "referendum")), "data.frame")
})

test_that("result of function", {
  expect_equal(class(get_filtered_votes(host = "192.168.137.38",
    deputies = c("Kopacz Ewa", "Palikot Janusz"))), "data.frame")
})

test_that("columns of table", {
  expect_equal(ncol(get_filtered_votes(host = "192.168.137.38", 
    meetings = c(1L,2L))), 8)
})

test_that("columns of table", {
  expect_equal(ncol(get_filtered_votes(host = "192.168.137.38",
    votings = c(100L,100L))), 8)
})

test_that("rows of table", {
  expect_more_than(nrow(get_filtered_votes(host = "192.168.137.38",
    clubs = c("PO","PiS"), dates = c("2014-01-01","2014-12-31"),
    topics = "referendum", deputies = c("Kopacz Ewa", "Rostowski"),
    meetings = c(1L,100L), votings = c(1L,200L))), 0)
})