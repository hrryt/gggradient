test_that("supports geom_area() and geom_ribbon()", {
  library(ggplot2)
  df <- data.frame(
    g = c("a", "a", "a", "b", "b", "b"),
    x = c(1, 3, 5, 2, 4, 6),
    y = c(2, 5, 1, 3, 6, 7)
  )
  a <- ggplot(df, aes(x, y, fill = g)) +
    geom_area()
  expect_doppelganger(
    "geom_area(x)",
    a + scale_fill_x(group = FALSE)
  )
  expect_doppelganger(
    "geom_area(y)",
    a + scale_fill_y(group = FALSE)
  )
  b <- ggplot(df, aes(x, ymin = y, ymax = 2 * y, group = g, fill = 1)) +
    geom_ribbon()
  expect_doppelganger(
    "geom_ribbon(x)",
    b + scale_fill_x(group = FALSE)
  )
  expect_doppelganger(
    "geom_ribbon(y)",
    b + scale_fill_y(group = FALSE)
  )
})

test_that("supports geom_bin_2d()", {
  library(ggplot2)
  d <- ggplot(diamonds, aes(x, y, fill = 1)) + xlim(4, 10) + ylim(4, 10) +
    geom_bin_2d(na.rm = TRUE, bins = 20)
  expect_doppelganger(
    "geom_bin_2d(x)",
    d + scale_fill_x(na.rm = TRUE)
  )
  expect_doppelganger(
    "geom_bin_2d(y)",
    d + scale_fill_y(na.rm = TRUE)
  )
})

test_that("supports geom_bar() and geom_col()", {
  library(ggplot2)
  expect_doppelganger(
    "geom_bar()",
    ggplot(mpg, aes(class, fill = 1)) +
      geom_bar() +
      scale_fill_y()
  )
  g <- ggplot(mpg, aes(class, group = year, fill = 1)) +
    scale_fill_y()
  expect_doppelganger(
    "geom_bar(stack)",
    g + geom_bar()
  )
  expect_doppelganger(
    "geom_bar(dodge)",
    g + geom_bar(position = "dodge")
  )
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  expect_doppelganger(
    "geom_col()",
    ggplot(df, aes(trt, outcome, fill = 1)) +
      geom_col() +
      scale_fill_y()
  )
})

test_that("supports geom_blank()", {
  library(ggplot2)
  expect_doppelganger(
    "geom_blank()",
    ggplot(mtcars, aes(wt, mpg, fill = 1)) +
      scale_fill_x()
  )
})

test_that("supports geom_boxplot()", {
  library(ggplot2)
  p <- ggplot(mpg, aes(class, hwy, fill = 1)) +
    geom_boxplot()
  expect_doppelganger(
    "geom_boxplot()",
    p + scale_fill_y(group = FALSE)
  )
})

test_that("supports geom_contour_filled", {
  library(ggplot2)
  v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
  expect_doppelganger(
    "geom_contour_filled()",
    v + geom_contour_filled(binwidth = 0.005) + scale_fill_x()
  )
})

test_that("supports geom_crossbar()", {
  library(ggplot2)
  df <- data.frame(
    trt = factor(c(1, 1, 2, 2)),
    resp = c(1, 5, 3, 4),
    group = factor(c(1, 2, 1, 2)),
    upper = c(1.1, 5.3, 3.3, 4.2),
    lower = c(0.8, 4.6, 2.4, 3.6)
  )
  p <- ggplot(df, aes(trt, resp, colour = group))
  expect_doppelganger(
    "geom_crossbar()",
    p + geom_crossbar(aes(ymin = lower, ymax = upper, fill = 1), width = 0.2) +
      scale_fill_y()
  )
})

test_that("supports geom_density()", {
  library(ggplot2)
  d <- ggplot(mpg, aes(hwy, fill = 1)) +
    geom_density()
  expect_doppelganger(
    "geom_density(x)",
     d + scale_fill_x_binned()
  )
  expect_doppelganger(
    "geom_density(y)",
    d + scale_fill_y_binned()
  )
})

test_that("supports geom_density_2d_filled()", {
  library(ggplot2)
  m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
    xlim(0.5, 6) +
    ylim(40, 110)
  expect_doppelganger(
    "geom_density_2d_filled()",
    m + geom_density_2d_filled() + scale_fill_y()
  )
})

test_that("supports geom_dotplot()", {

})

test_that("supports geom_hex()", {
  skip_if_not_installed("hexbin")
  library(ggplot2)
  d <- ggplot(diamonds, aes(carat, price)) +
    geom_hex(bins = 10)
  expect_doppelganger(
    "geom_hex(x)",
    d + scale_fill_x(limits = c(1, 2))
  )
  expect_doppelganger(
    "geom_hex(y)",
    d + scale_fill_y(limits = c(5e3, 1e4))
  )
})

test_that("supports geom_histogram()", {
  library(ggplot2)
  d <- ggplot(diamonds, aes(carat, fill = 1)) +
    geom_histogram(bins = 10)
  expect_doppelganger(
    "geom_histogram(x)",
    d + scale_fill_x(limits = c(1, 2))
  )
  expect_doppelganger(
    "geom_histogram(y)",
    d + scale_fill_y(limits = c(5e3, 1e4))
  )
})

test_that("supports geom_label()" , {

})

test_that("supports geom_map() and expand_limits()", {

})

test_that("supports geom_point()", {

})

test_that("supports geom_polygon()", {
  library(ggplot2)
  ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

  values <- data.frame(
    id = ids,
    value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
  )

  positions <- data.frame(
    id = rep(ids, each = 4),
    x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
          0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
          2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
  )

  # Currently we need to manually merge the two together
  datapoly <- merge(values, positions, by = c("id"))

  p <- ggplot(datapoly, aes(x = x, y = y)) +
    geom_polygon(aes(fill = value, group = id))

  expect_doppelganger(
    "geom_polygon()",
    p + scale_fill_y()
  )

  skip_if_not_installed("grid", minimum_version = "3.6")

  # As of R version 3.6 geom_polygon() supports polygons with holes
  # Use the subgroup aesthetic to differentiate holes from the main polygon

  holes <- do.call(rbind, lapply(split(datapoly, datapoly$id), function(df) {
    df$x <- df$x + 0.5 * (mean(df$x) - df$x)
    df$y <- df$y + 0.5 * (mean(df$y) - df$y)
    df
  }))
  datapoly$subid <- 1L
  holes$subid <- 2L
  datapoly <- rbind(datapoly, holes)

  p <- ggplot(datapoly, aes(x = x, y = y)) +
    geom_polygon(aes(fill = value, group = id, subgroup = subid))

  expect_doppelganger(
    "geom_polygon(holes)",
    p + scale_fill_y()
  )
})

test_that("supports geom_rect() and geom_tile()", {
  library(ggplot2)
  df <- data.frame(
    x = rep(c(2, 5, 7, 9, 12), 2),
    y = rep(c(1, 2), each = 5),
    z = factor(rep(1:5, each = 2)),
    w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
  )
  expect_doppelganger(
    "geom_rect()",
    ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
      geom_rect(aes(fill = z), colour = "grey50") + scale_fill_y()
  )
  expect_doppelganger(
    "geom_tile()",
    ggplot(df, aes(x, y, width = w, fill = 1)) +
      geom_tile(colour = "grey50") +
      scale_fill_x()
  )
})

test_that("supports geom_sf()", {
  skip_if_not_installed("sf")
  library(ggplot2)
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  expect_doppelganger(
    "geom_sf()",
    ggplot(nc) +
      geom_sf(aes(fill = 1)) +
      scale_fill_x()
  )
})

test_that("supports geom_smooth()", {
  library(ggplot2)
  expect_doppelganger(
    "geom_smooth()",
    ggplot(mpg, aes(displ, hwy, fill = 1)) +
      geom_smooth(method = "loess", formula = y ~ x) +
      scale_fill_x()
  )
})

test_that("supports geom_violin()", {
  library(ggplot2)
  expect_doppelganger(
    "geom_violin()",
    ggplot(mtcars, aes(mpg, factor(cyl), fill = 1)) +
      geom_violin() +
      scale_fill_x(group = FALSE)
  )
})
