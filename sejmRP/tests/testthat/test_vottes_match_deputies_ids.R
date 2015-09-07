test_that("result of function", {
  expect_equal(class(votes_match_deputies_ids("sejmrp", "sejmrp", "password", "192.168.137.38",
    "http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=klubglos&IdGlosowania=37494&KodKlubu=PO",
    .Platform$OS.type == "windows")),
    "data.frame")
})

test_that("columns of table", {
  expect_equal(ncol(votes_match_deputies_ids("sejmrp", "sejmrp", "password", "192.168.137.38",
    "http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=klubglos&IdGlosowania=37494&KodKlubu=SLD",
    .Platform$OS.type == "windows")), 3)
})

test_that("rows of table", {
  expect_more_than(nrow(votes_match_deputies_ids("sejmrp", "sejmrp", "password", "192.168.137.38",
    "http://www.sejm.gov.pl/Sejm7.nsf/agent.xsp?symbol=klubglos&IdGlosowania=37494&KodKlubu=PiS",
    .Platform$OS.type == "windows")), 0)
})