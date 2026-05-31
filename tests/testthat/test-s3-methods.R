test_that("S3 print and plot methods work", {

  skip_on_cran()

  net <- get_cycling_network("Muenster, Germany", bbox_km = 1)
  cl <- classify_bike_infrastructure(net)

  # print methods (no error expected)
  expect_error(print(net), NA)
  expect_error(print(cl), NA)

  # plot methods (should return ggplot)
  expect_s3_class(plot(net), "ggplot")
  expect_s3_class(plot(cl), "ggplot")
})