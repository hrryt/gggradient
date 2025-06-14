test_that("suports new_scale()", {
  skip_if_not_installed("ggnewscale")
  library(ggplot2)
  expect_doppelganger(
    "new_scale_fill()",
    ggplot(mpg, aes(class, fill = class)) +
      geom_bar() +
      ggnewscale::new_scale_fill() +
      geom_bar(aes(fill = 1)) +
      scale_fill_y(limits = c(20, 40), na.value = NA)
  )
})
