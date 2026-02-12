test_that("pal.info data exists and has correct structure", {
  skip_if_not_installed("pyShinyCell")

  # Load the package data
  data("pal.info", package = "pyShinyCell")

  # Check structure
  expect_s3_class(pal.info, "data.frame")
  expect_true(nrow(pal.info) > 0)
  expect_true(all(c("maxcolors", "package") %in% colnames(pal.info)))

  # Check known palettes exist
  expect_true("Set1" %in% rownames(pal.info))
})

test_that("g_legend extracts legend from ggplot", {
  skip_if_not_installed("ggplot2")

  # Create a simple ggplot with a legend
  df <- data.frame(x = 1:5, y = 1:5, group = rep(c("A", "B"), c(2, 3)))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = group)) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "bottom")

  # Extract legend
  legend <- g_legend(p)

  expect_s3_class(legend, "grob")
})

test_that("sctheme provides a valid ggplot2 theme", {
  skip_if_not_installed("ggplot2")

  # Create theme
  theme <- sctheme()

  # Should return a theme object that can be added to a plot
  expect_s3_class(theme, "theme")
})
