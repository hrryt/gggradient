#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.gggradient_scale <- function(object, plot, object_name) {
  half_built <- suppressMessages(half_build(plot))
  position_scale <- half_built$scales$get_scales(object$position)
  object <- construct_scale(object, position_scale)
  plot$scales$add(object)
  plot
}

half_build <- function(plot) {
  plot <- plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + ggplot2::geom_blank()
  }
  layers <- plot$layers
  data <- rep(list(NULL), length(layers))
  scales <- plot$scales
  data <- by_layer(function(l, d) l$layer_data(plot$data),
                   layers, data, "computing layer data")
  data <- by_layer(function(l, d) l$setup_layer(d, plot),
                   layers, data, "setting up layer")
  layout <- create_layout(plot$facet, plot$coordinates, plot$layout)
  data <- layout$setup(data, plot$data, plot$plot_env)
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot),
                   layers, data, "computing aesthetics")
  data <- ggplot2::.ignore_data(data)
  data <- lapply(data, scales$transform_df)
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)
  data <- ggplot2::.expose_data(data)
  data <- by_layer(function(l, d) l$compute_statistic(d, layout),
                   layers, data, "computing stat")
  data <- by_layer(function(l, d) l$map_statistic(d, plot),
                   layers, data, "mapping stat to aesthetics")
  plot$scales$add_missing(c("x", "y"), plot$plot_env)
  plot
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p
}

by_layer <- function(f, layers, data, step = NULL) {
  ordinal <- scales::label_ordinal()
  out <- vector("list", length(data))
  rlang::try_fetch(for (i in seq_along(data)) {
    out[[i]] <- f(l = layers[[i]], d = data[[i]])
  }, error = function(cnd) {
    cli::cli_abort(
      c("Problem while {step}.",
        i = "Error occurred in the {ordinal(i)} layer."),
      call = layers[[i]]$constructor, parent = cnd
    )
  })
  out
}

create_layout <- function(facet, coord, layout = NULL) {
  layout <- layout %||% ggplot2::Layout
  # check_inherits(layout, "Layout")
  ggplot2::ggproto(NULL, layout, facet = facet, coord = coord)
}
