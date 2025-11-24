library(testthat)
library(ggplot2)
source("R/ggtranslate.R")


test_that("ggtranslate correctly translates plot titles, labels, and captions", {
  p_en <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() +
    labs(
      title = "My Title",
      subtitle = "My Subtitle",
      caption = "My Caption",
      x = "X Axis",
      y = "Y Axis"
    )

  dict <- list(
    "My Title" = "Mon Titre",
    "My Subtitle" = "Mon Sous-titre",
    "My Caption" = "Ma Légende",
    "X Axis" = "Axe X",
    "Y Axis" = "Axe Y"
  )

  p_fr <- ggtranslate(p_en, dict)

  expect_equal(p_fr$labels$title, "Mon Titre")
  expect_equal(p_fr$labels$subtitle, "Mon Sous-titre")
  expect_equal(p_fr$labels$caption, "Ma Légende")
  expect_equal(p_fr$labels$x, "Axe X")
  expect_equal(p_fr$labels$y, "Axe Y")
})


test_that("ggtranslate correctly translates discrete scale labels", {
  df <- data.frame(x = c("One", "Two"), y = c(1, 2))
  p_en <- ggplot(df, aes(x, y, color = x)) +
    geom_point()

  dict <- list(
    "One" = "Un",
    "Two" = "Deux"
  )
  p_fr <- ggtranslate(p_en, dict)

  # Check x-axis scale
  x_labels <- ggplot_build(p_fr)$layout$panel_scales_x[[1]]$get_labels()
  expect_equal(x_labels, c("Un", "Deux"))

  # Check color scale
  color_labels <- p_fr$scales$get_scales("colour")$get_labels()
  expect_equal(color_labels, c("Un", "Deux"))
})

test_that("ggtranslate correctly translates facet labels", {
  df <- data.frame(x = c("One", "Two"), y = c(1, 2), facet = c("Facet 1", "Facet 2"))
  p_en <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_wrap(~facet)

  dict <- list("Facet 1" = "Facette 1", "Facet 2" = "Facette 2")

  p_fr <- ggtranslate(p_en, dict)

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

  expect_false(is.null(p_fr$layers)) #

  ## check all layers
  for (lll in p_fr$layers) {
    mapped_var <- lll$mapping$label ## we dont change computed_mapping
    mapped_var <- ifelse(is.null(mapped_var), "", mapped_var |> rlang::as_name())
    expect_equal(p_fr$data[[mapped_var]], c("ccc", "ddd"))
  }
})


## TEST NUMERIC TEXT/LABELS
test_that("ggtranslate doesn't translate numeric labels", {
  df <- data.frame(name = c("One", "Two"), count = c(1, 2))
  p_en <- ggplot(df, aes(x = name, y = count, label = count)) +
    geom_text(aes(label = count)) +
    geom_label(vjust = 2)

  dict <- list("One" = "Un", "Two" = "Deux")
  p_fr <- ggtranslate(p_en, dict)

  expect_equal(length(names(p_fr$data)), 2)
  ## count_translated is usually a column added by ggtranslate, here we dont want it
  expect_false("count_translated" %in% names(p_fr$data))
})


## TEST FORMULA TEXT/LABELS
test_that("ggtranslate can translate composite labels", {
  df <- data.frame(name1 = c("One", "Two"), name2 = c("Three", "Four"), count = c(1, 2))
  p_en <- ggplot(df, aes(x = name1, y = count, label = paste(name1, name2, sep = " - "))) +
    geom_text(aes(label = paste(name1, name2, sep = " - "))) +
    geom_label(vjust = 2)

  dict <- list("One - Three" = "Un - Trois", "Two - Four" = "Deux - Quatre")
  p_fr <- ggtranslate(p_en, dict)
  expect_false(is.null(p_fr$layers)) #

  ## check all layers
  for (lll in p_fr$layers) {
    mapped_var <- lll$mapping$label ## we dont change computed_mapping
    mapped_var <- ifelse(is.null(mapped_var), "", mapped_var |> rlang::as_name())
    expect_equal(p_fr$data[[mapped_var]], c("Un - Trois", "Deux - Quatre"))
  }
})


test_that("ggtranslate correctly translates new ggplot2 v4.0.0 label attributes, 1st method", {
  ## taken from https://tidyverse.org/blog/2025/09/ggplot2-4-0-0/#labels

  # The penguins dataset was incorporated into base R 4.5
  df <- penguins

  # Manually set label attributes.
  attr(df$species, "label") <- "Penguin Species"
  attr(df$bill_dep, "label") <- "Bill depth (mm)"
  attr(df$bill_len, "label") <- "Bill length (mm)"
  attr(df$body_mass, "label") <- "Body mass (g)"

  p_en <- ggplot(df, aes(bill_dep, bill_len, colour = body_mass)) +
    geom_point(na.rm = TRUE)

  dict <- list(
    "Penguin Species" = "Espèces de manchots",
    "Bill depth (mm)" = "Profondeur du bec (mm)",
    "Bill length (mm)" = "Longueur du bec (mm)",
    "Body mass (g)" = "Masse corporelle (g)"
  )
  p_fr <- ggtranslate(p_en, dict)


  expect_equal(p_fr$labels$x, dict[["Bill depth (mm)"]])
  expect_equal(p_fr$labels$y, dict[["Bill length (mm)"]])
  expect_equal(p_fr$labels$colour, dict[["Body mass (g)"]])
})

test_that("ggtranslate correctly translates new ggplot2 v4.0.0 label attributes, 2nd method", {
  dict <- tibble::tribble(
    ~var, ~label,
    "species", "Penguin Species",
    "bill_dep", "Bill depth (mm)",
    "bill_len", "Bill length (mm)",
    "body_mass", "Body mass (g)"
  )
  p_en <- ggplot(penguins, aes(bill_dep, bill_len, colour = body_mass)) +
    geom_point(na.rm = TRUE) +
    # Or:
    # labs(dictionary = dplyr::pull(dict, label, name = var))
    labs(dictionary = setNames(dict$label, dict$var))

  dict <- list(
    "Penguin Species" = "Espèces de manchots",
    "Bill depth (mm)" = "Profondeur du bec (mm)",
    "Bill length (mm)" = "Longueur du bec (mm)",
    "Body mass (g)" = "Masse corporelle (g)"
  )
  p_fr <- ggtranslate(p_en, dict)


  expect_equal(p_fr$labels$x, dict[["Bill depth (mm)"]])
  expect_equal(p_fr$labels$y, dict[["Bill length (mm)"]])
  expect_equal(p_fr$labels$colour, dict[["Body mass (g)"]])
})
