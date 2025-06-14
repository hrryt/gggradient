#' @export
print.gggradient_scale <- function(x, ...) {
  cat("<gggradient_scale>\n")
  print(x$gradient_scale)
  invisible(x)
}

new_gggradient_scale <- function(gradient_scale, position, stops, na.rm,
                                group, position_aes) {
  structure(
    list(
      gradient_scale = gradient_scale, position = position,
      stops = stops, na.rm = na.rm, group = group, position_aes = position_aes
    ),
    class = "gggradient_scale"
  )
}
