test_that("classify_bike_infrastructure creates correct object", {

  skip_on_cran()

  net <- get_cycling_network("Muenster, Germany", bbox_km = 1)
  cl <- classify_bike_infrastructure(net)

  expect_s3_class(cl, "cycling_classification")
  expect_s3_class(cl, "cycling_network")

  expect_true("infra_type" %in% names(cl$classified))
  expect_true("summary" %in% names(cl))
  expect_true(nrow(cl$summary) > 0)
})