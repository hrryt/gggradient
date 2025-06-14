write_svg_bleeding_edge <- function(plot, file, title = "") {
  # use Liberation Sans and Symbola to avoid platform-specific font differences
  liberation_sans <- fontquiver::font_styles("Liberation", "Sans")
  symbola <- fontquiver::font("Symbola", "Symbols", "Regular")
  sysfonts::font_add(
    "Liberation Sans",
    regular = liberation_sans$Regular$ttf,
    bold = liberation_sans$Bold$ttf,
    italic = liberation_sans$Italic$ttf,
    bolditalic = liberation_sans$`Bold Italic`$ttf,
    symbol = symbola$ttf
  )

  svglite::svglite(file)
  showtext::showtext_begin()
  on.exit({
    showtext::showtext_end()
    grDevices::dev.off()
  })

  print(
    plot +
      ggplot2::ggtitle(title) +
      ggplot2::theme_test(base_family = "Liberation Sans")
  )
}

expect_doppelganger <- function(...) {
  vdiffr::expect_doppelganger(..., writer = write_svg_bleeding_edge)
}
