p = wiDB_values("projects")

test_that("wiDB_values works", {
  expect_type(p, "list")
  expect_length(p, 1)
})

p = wiDB_sites(minDate = "2020-09-01", types = "Tap")

test_that("wiDB_sites works", {
  expect_s3_class(p, "data.frame")
  expect_gte(nrow(p), 2)
})

p = wiDB_data(minDate = "1990-01-01", maxDate = "1990-02-01", 
                     countries = "US", types = "Precipitation")

test_that("wiDB_data works", {
  expect_type(p, "list")
  expect_length(p, 2)
})
