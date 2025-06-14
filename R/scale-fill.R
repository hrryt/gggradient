#' Gradient fill scales
#'
#' Scales to map continuous `x` or `y` data values onto the `fill` aesthetic,
#' producing a gradient fill along the respective axis.
#'
#' These scales use information from their respective positional scales,
#' so any `x` or `y` scale variants should be added to the plot beforehand.
#'
#' Bounding ranges of grobs or shapes are guessed based on the data values of
#' the aesthetics of the positional scale, with the exception that if `lower`,
#' `upper`, `xlower` or `xupper` are present, no other aesthetics are used.
#' This can be overridden by specifying `bounding.aes`. See examples.
#'
#' Does not yet work for `geom_point()` and similar geometries, including
#' `geom_label()`, or for non-linear coordinate systems.
#'
#' @inheritParams ggplot2::scale_fill_continuous
#' @inheritParams grid::linearGradient
#' @inheritParams ggplot2::geom_bar
#'
#' @param n.colours Number of colours for the gradient to transition between.
#' @param bounding.aes Character vector of position aesthetics for guessing
#' bounding boxes. See Details.
#'
#' @returns An object of class `gggradient_scale`. For internal use only.
#' @seealso [ggplot2::scale_fill_continuous()], [ggplot2::scale_x_continuous()].
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg, aes(class, fill = 1)) +
#'   geom_bar(colour = "black") +
#'   scale_fill_y()
#'
#' # set aes(fill) only for geoms to be affected
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_smooth(aes(fill = 1), alpha = 1) +
#'   geom_point()
#'
#' p + scale_fill_x_viridis_b()
#' p + scale_fill_x_viridis_b(n.colours = 20) # set resolution
#'
#' g <- ggplot(mpg, aes(hwy, class, fill = 1))
#' b <- g + geom_boxplot()
#'
#' # some geoms require group = FALSE
#' b + scale_fill_x_viridis_c(group = FALSE)
#' b + scale_fill_x_viridis_c(group = TRUE)
#'
#' # use scale_fill_x|y_*() as you would scale_fill_*()
#' b +
#'   scale_fill_x_distiller(
#'     name = "hwy",
#'     limits = c(25, 30),
#'     na.value = NA,
#'     palette = "RdBu",
#'     direction = 1,
#'     group = FALSE
#'   )
#'
#' # notchupper and notchlower are not recognised as positional aesthetics,
#' # so must be specified if notches exceed the range of their hinges.
#' # see the documentation for the computed variables of stat_boxplot()
#' g + geom_boxplot(notch = TRUE) +
#'   scale_fill_x_viridis_c(group = FALSE, bounding.aes = c(
#'     "xlower", "xupper", "notchlower", "notchupper"
#'   ))
scale_fill_x <- function(..., type = getOption("ggplot2.continuous.fill"),
                         n.colours = 500, na.rm = FALSE, group = TRUE,
                         bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_continuous(..., type = type)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y <- function(..., type = getOption("ggplot2.continuous.fill"),
                         n.colours = 500, na.rm = FALSE, group = TRUE,
                         bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_continuous(..., type = type)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_continuous <- scale_fill_x

#' @rdname scale_fill_x
#' @export
scale_fill_y_continuous <- scale_fill_y

#' @rdname scale_fill_x
#' @export
scale_fill_x_binned <- function(..., type = getOption("ggplot2.binned.fill"),
                                n.colours = 500, na.rm = FALSE, group = TRUE,
                                bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_binned(..., type = type)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_binned <- function(..., type = getOption("ggplot2.binned.fill"),
                                n.colours = 500, na.rm = FALSE, group = TRUE,
                                bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_binned(..., type = type)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_date <- function(...,
                              n.colours = 500, na.rm = FALSE, group = TRUE,
                              bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_date(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_date <- function(...,
                              n.colours = 500, na.rm = FALSE, group = TRUE,
                              bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_date(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_datetime <- function(...,
                                  n.colours = 500, na.rm = FALSE, group = TRUE,
                                  bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_datetime(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_datetime <- function(...,
                                  n.colours = 500, na.rm = FALSE, group = TRUE,
                                  bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_datetime(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_fermenter <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_fermenter(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_fermenter <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_fermenter(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_distiller <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_distiller(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_distiller <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_distiller(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_gradient <- function(...,
                                  n.colours = 500, na.rm = FALSE, group = TRUE,
                                  bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_gradient <- function(...,
                                  n.colours = 500, na.rm = FALSE, group = TRUE,
                                  bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_gradient2 <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient2(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_gradient2 <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient2(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_gradientn <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradientn(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_gradientn <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradientn(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_steps <- function(...,
                               n.colours = 500, na.rm = FALSE, group = TRUE,
                               bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_steps <- function(...,
                               n.colours = 500, na.rm = FALSE, group = TRUE,
                               bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_steps2 <- function(...,
                                n.colours = 500, na.rm = FALSE, group = TRUE,
                                bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps2(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_steps2 <- function(...,
                                n.colours = 500, na.rm = FALSE, group = TRUE,
                                bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps2(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_stepsn <- function(...,
                                n.colours = 500, na.rm = FALSE, group = TRUE,
                                bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_stepsn(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_stepsn <- function(...,
                                n.colours = 500, na.rm = FALSE, group = TRUE,
                                bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_stepsn(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_viridis_c <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_c(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_viridis_c <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_c(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_x_viridis_b <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_b(...)
  new_gggradient_scale(
    gradient_scale, "x", n.colours, na.rm, group, bounding.aes
  )
}

#' @rdname scale_fill_x
#' @export
scale_fill_y_viridis_b <- function(...,
                                   n.colours = 500, na.rm = FALSE, group = TRUE,
                                   bounding.aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_b(...)
  new_gggradient_scale(
    gradient_scale, "y", n.colours, na.rm, group, bounding.aes
  )
}
