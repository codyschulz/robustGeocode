# Test geocode_robust()

test_that("geocode_robust_1", {
  data <- data.frame(addr = c("The White House", "11 Wall St, New York, NY", "525 A St. NE, Washington, DC", "42 Wallaby Way, Sydney"),
                     stringsAsFactors = FALSE)

  results <- geocode_robust(data, location = "addr")

  expect_that(
    all(!is.na(results$lon)) & all(!is.na(results$lat)),
    is_true())
})

test_that("geocode_robust_2", {
  data <- data.frame(addr = c("The White House", "11 Wall St, New York, NY", "525 A St. NE, Washington, DC", "42 Wallaby Way, Sydney"),
                     stringsAsFactors = FALSE)
  data1 <- rbind(data, data[1,])

  results <- geocode_robust(data1, location = "addr")

  expect_that(
    nrow(results) == nrow(data1),
    is_true()
  )
})

test_that("geocode_robust_3", {

  data <- data.frame(addr = c("The White House", "11 Wall St, New York, NY", "525 A St. NE, Washington, DC", "42 Wallaby Way, Sydney"),
                     stringsAsFactors = FALSE)

  results <- geocode_robust(data, location = "addr", return_full_dataframe = TRUE)

  expect_that(
    !is.null(results$lon),
    is_true()
  )

  expect_that(
    identical(results[, !grepl("lon|lat", colnames(results)), drop = FALSE], data),
    is_true())

})
