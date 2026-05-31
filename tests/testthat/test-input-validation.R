test_that("input validation works", {

  expect_error(get_cycling_network("", bbox_km = 5))
  expect_error(get_cycling_network("Muenster", crs = "abc"))

  net <- get_cycling_network("Muenster, Germany", bbox_km = 1)

  expect_error(classify_bike_infrastructure("not a network"))
  expect_error(plot_cycling_safety_map("not a classification"))
})