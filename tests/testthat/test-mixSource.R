O = runif(3, -15, -2)
H = O * 8 + 10 + rnorm(3, 0, 6)
sources = iso(H, O, 1, 0.2, 0.17)

test_that("iso works", {
  expect_s3_class(sources, c("data.frame", "iso"))
})

obs = iso(-60, -6, 0.5, 0.1, 0)
slope = c(5, 0.3)

test_that("mixSource works", {
  expect_length(mixSource(obs, sources, slope, ngens = 1e2), 2)
  expect_length(mixSource(obs, sources, slope, ngens = 1e2, ncores = 2), 2)
})

obs2 = obs
class(obs2) = "data.frame"
sources2 = sources
class(sources2)[2] = "duh"

test_that("mixSource warnings work", {
  expect_warning(mixSource(obs2, sources, slope, ngens = 1e2))
  expect_warning(mixSource(obs, sources2, slope, ngens = 1e2))
})

test_that("mixSource errors work", {
  expect_error(mixSource(obs, sources[1,], slope))
})
