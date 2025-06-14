construct_scale <- function(object, position_scale) {
  ggplot2::check_device(feature = "patterns")

  force(object)
  force(position_scale)

  ggplot2::ggproto(
    "ScaleContinuousGradient", object$gradient_scale,
    train_df = function(self, df) {
      if (empty(df))
        return()
      aesthetics <- intersect(position_scale$aesthetics, names(df))
      for (aesthetic in aesthetics) {
        transed <- self$transform(position_scale$trans$inverse(df[[aesthetic]]))
        self$train(transed)
      }
      invisible()
    },
    map_df = function(self, df, i = NULL) {
      if (empty(df)) {
        return()
      }

      gradient_aes <- intersect(self$aesthetics, names(df))
      names(gradient_aes) <- gradient_aes

      if (length(gradient_aes) == 0) {
        return()
      }

      if (!is.null(i)) {
        df <- df[i, ]
      }

      if (object$group) {
        df$group <- -1
      }

      x1 <- x2 <- y1 <- y2 <- 0
      if (any(position_scale$aesthetics == "x")) {
        if (!is.null(df$width) && is.null(df$xmin) && is.null(df$xmax)) {
          df$xmin <- df$x - df$width / 2
          df$xmax <- df$x + df$width / 2
        }
        x2 <- 1
      } else if (any(position_scale$aesthetics == "y")) {
        if (!is.null(df$height) && is.null(df$ymin) && is.null(df$ymax)) {
          df$ymin <- df$y - df$height / 2
          df$ymax <- df$y + df$height / 2
        }
        y2 <- 1
      } else {
        return()
      }

      position_aes <- object$position_aes
      if (is.null(position_aes)) {
        position_aes <- intersect(position_scale$aesthetics, names(df))
        lmu <- position_aes %in% c("lower", "middle", "upper")
        if (any(lmu)) {
          position_aes <- position_aes[lmu]
        }
      }
      names(position_aes) <- position_aes

      if(length(position_aes) == 0) {
        return()
      }

      position_df <- df[, position_aes, drop = FALSE]

      gradients <- lapply(split(position_df, df$group), function(df) {
        nrow <- nrow(df)
        df <- ggplot2::remove_missing(
          df, na.rm = object$na.rm, name = "scale_fill_position"
        )
        if (empty(df)) return()

        lower <- min(vapply(df, min, numeric(1L), na.rm = TRUE), na.rm = TRUE)
        upper <- max(vapply(df, max, numeric(1L), na.rm = TRUE), na.rm = TRUE)
        bounds <- c(lower, upper)

        scaled <- position_scale$rescaler(object$stops, to = bounds)
        transed <- self$transform(position_scale$trans$inverse(scaled))
        mapped <- self$map(transed)

        gradient <- grid::linearGradient(
          mapped, object$stops,
          x1 = x1, x2 = x2, y1 = y1, y2 = y2
        )
        rep(list(gradient), nrow)
      })

      gradient_aes[] <- list(unlist(gradients, recursive = FALSE))
      gradient_aes
    }
  )
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}
