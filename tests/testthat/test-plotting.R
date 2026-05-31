test_that("plot_cycling_safety_map returns ggplot object", {

  skip_on_cran()

  net <- get_cycling_network("Muenster, Germany", bbox_km = 1)
  cl <- classify_bike_infrastructure(net)

  p <- plot_cycling_safety_map(cl, show_stats = FALSE)

  expect_s3_class(p, "ggplot")
})