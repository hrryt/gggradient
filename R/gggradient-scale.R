#' @export
print.gggradient_scale <- function(x, ...) {
  cat("<gggradient_scale>\n")
  print(x$gradient_scale)
  invisible(x)
}

new_gggradient_scale <- function(gradient_scale, position, n.colours, na.rm,
                                group, bounding.aes) {
  structure(
    list(
      gradient_scale = gradient_scale, position = position,
      n.colours = n.colours, na.rm = na.rm, group = group,
      bounding.aes = bounding.aes, call = rlang::caller_call()
    ),
    class = "gggradient_scale"
  )
}
