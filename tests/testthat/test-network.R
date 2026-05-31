test_that("get_cycling_network returns correct class", {

  skip_on_cran()

  net <- get_cycling_network("Muenster, Germany", bbox_km = 1)

  expect_s3_class(net, "cycling_network")
  expect_true(is.character(net$city))
  expect_s3_class(net$lines, "sf")
  expect_true(nrow(net$lines) > 0)
})