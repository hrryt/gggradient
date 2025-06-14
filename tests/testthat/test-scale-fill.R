test_that("supports transformations", {
  library(ggplot2)
  expect_doppelganger(
    "transformations",
    ggplot(mpg, aes(class, fill = 1)) +
      geom_bar() +
      scale_y_log10() +
      scale_fill_y(trans = "reverse", type = "viridis")
  )
})

test_that("supports limits", {
  library(ggplot2)
  expect_doppelganger(
    "scale limits",
    ggplot(mpg, aes(class, fill = 1)) +
      geom_bar() +
      ylim(NA, 100) +
      scale_fill_y(limits = c(25, 50), type = "viridis")
  )
})

test_that("supports binning", {
  library(ggplot2)
  expect_doppelganger(
    "binning",
    ggplot(mpg, aes(class, fill = 1)) +
      geom_bar() +
      scale_fill_y_binned(type = "viridis")
  )
})

test_that("supports guide labelling", {
  library(ggplot2)
  expect_doppelganger(
    "aes(after_stat)",
    ggplot(mpg, aes(class)) +
      geom_bar(aes(fill = after_stat(count))) +
      scale_fill_y(type = "viridis")
  )
})
