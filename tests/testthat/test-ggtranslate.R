library(testthat)
library(ggplot2)
# library(ggtranslate)

test_that("ggtranslate correctly translates plot titles, labels, and captions", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    labs(title = "My Title", subtitle = "My Subtitle", caption = "My Caption", x = "X Axis", y = "Y Axis")

  dict <- list(
    "My Title" = "Mon Titre",
    "My Subtitle" = "Mon Sous-titre",
    "My Caption" = "Ma Légende",
    "X Axis" = "Axe X",
    "Y Axis" = "Axe Y"
  )

  p_fr <- ggtranslate(p, dict)

  expect_equal(p_fr$labels$title, "Mon Titre")
  expect_equal(p_fr$labels$subtitle, "Mon Sous-titre")
  expect_equal(p_fr$labels$caption, "Ma Légende")
  expect_equal(p_fr$labels$x, "Axe X")
  expect_equal(p_fr$labels$y, "Axe Y")
})

# plot = p
# dictionary = dict

test_that("ggtranslate correctly translates discrete scale labels", {
  df <- data.frame(x = c("One", "Two"), y = c(1, 2))
  p <- ggplot(df, aes(x, y, color = x)) +
    geom_point()

  dict <- list(
    "One" = "Un",
    "Two" = "Deux"
  )
  p_fr <- ggtranslate(p, dict)

  # Check x-axis scale
  # x_labels <- p_fr$scales$get_scales("x")$get_labels() ## doesnt work
  x_labels <- ggplot_build(p_fr)$layout$panel_scales_x[[1]]$get_labels()
  expect_equal(x_labels, c("Un", "Deux"))

  # Check color scale
  color_labels <- p_fr$scales$get_scales("colour")$get_labels()
  expect_equal(color_labels, c("Un", "Deux"))
})

test_that("ggtranslate correctly translates facet labels", {
  df <- data.frame(x = c("One", "Two"), y = c(1, 2), facet = c("Facet 1", "Facet 2"))
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_wrap(~facet)

  dict <- list("Facet 1" = "Facette 1", "Facet 2" = "Facette 2")

  p_fr <- ggtranslate(p, dict)

  # Check the labeller function
  labeller <- p_fr$facet$params$labeller
  expect_true(is.function(labeller))

  # Test the labeller
  df_test <- data.frame(facet = c("Facet 1", "Facet 2"))
  translated_labels <- labeller(df_test)
  expect_equal(as.character(translated_labels$facet), c("Facette 1", "Facette 2"))
})

test_that("ggtranslate correctly translates geom_text and geom_label labels", {
  df <- data.frame(x = c(1, 2), y = c(3, 4), txt = c("aaaaa", "bbbbb"))
  p_en <- ggplot(df, aes(x, y, label = txt)) +
    geom_label(x = 1.5, aes(label = txt)) + ## explicit aes
    geom_text() ## inherited aes

  dict <- list("aaaaa" = "ccc", "bbbbb" = "ddd")
  p_fr <- ggtranslate(p_en, dict)
  # plot(p_fr)

  ## check all layers
  for (lll in p_fr$layer) {
    mapped_var <- lll$mapping$label ## we dont change computed_mapping
    mapped_var <- ifelse(is.null(mapped_var), "", mapped_var |> rlang::as_name())
    # print(mapped_var)
    expect_equal(p_fr$data[[mapped_var]], c("ccc", "ddd"))
  }
})
