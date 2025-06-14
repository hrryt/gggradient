#' @export
scale_fill_x <- function(..., type = getOption("ggplot2.continuous.fill"),
                         stops = seq(0, 1, length.out = nstops),
                         nstops = 500, na.rm = FALSE, group = TRUE,
                         position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_continuous(..., type = type)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y <- function(..., type = getOption("ggplot2.continuous.fill"),
                         stops = seq(0, 1, length.out = nstops),
                         nstops = 500, na.rm = FALSE, group = TRUE,
                         position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_continuous(..., type = type)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_continuous <- scale_fill_x

#' @export
scale_fill_y_continuous <- scale_fill_y

#' @export
scale_fill_x_binned <- function(..., type = getOption("ggplot2.continuous.fill"),
                         stops = seq(0, 1, length.out = nstops),
                         nstops = 500, na.rm = FALSE, group = TRUE,
                         position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_binned(..., type = type)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_binned <- function(..., type = getOption("ggplot2.continuous.fill"),
                                stops = seq(0, 1, length.out = nstops),
                                nstops = 500, na.rm = FALSE, group = TRUE,
                                position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_binned(..., type = type)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_date <- function(..., stops = seq(0, 1, length.out = nstops),
                              nstops = 500, na.rm = FALSE, group = TRUE,
                              position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_date(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_date <- function(..., stops = seq(0, 1, length.out = nstops),
                              nstops = 500, na.rm = FALSE, group = TRUE,
                              position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_date(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_datetime <- function(..., stops = seq(0, 1, length.out = nstops),
                              nstops = 500, na.rm = FALSE, group = TRUE,
                              position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_datetime(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_datetime <- function(..., stops = seq(0, 1, length.out = nstops),
                              nstops = 500, na.rm = FALSE, group = TRUE,
                              position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_datetime(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_fermenter <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_fermenter(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_fermenter <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_fermenter(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_distiller <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_distiller(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_distiller <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_distiller(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_gradient <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_gradient <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_gradient2 <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient2(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_gradient2 <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradient2(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_gradientn <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradientn(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_gradientn <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_gradientn(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}
#' @export
scale_fill_x_steps <- function(..., stops = seq(0, 1, length.out = nstops),
                               nstops = 500, na.rm = FALSE, group = TRUE,
                               position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_steps <- function(..., stops = seq(0, 1, length.out = nstops),
                                  nstops = 500, na.rm = FALSE, group = TRUE,
                                  position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_steps2 <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps2(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_steps2 <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_steps2(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_stepsn <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_stepsn(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_stepsn <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_stepsn(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_viridis_c <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_c(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_viridis_c <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_c(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_x_viridis_b <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_b(...)
  new_gggradient_scale(
    gradient_scale, "x", stops, na.rm, group, position_aes
  )
}

#' @export
scale_fill_y_viridis_b <- function(..., stops = seq(0, 1, length.out = nstops),
                                   nstops = 500, na.rm = FALSE, group = TRUE,
                                   position_aes = NULL) {
  gradient_scale <- ggplot2::scale_fill_viridis_b(...)
  new_gggradient_scale(
    gradient_scale, "y", stops, na.rm, group, position_aes
  )
}
