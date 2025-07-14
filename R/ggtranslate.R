#' Translate all text elements in a ggplot2 object
#'
#' This function takes a ggplot2 object and a named list to translate
#' all user-facing text elements within the plot. This includes main plot labels
#' (title, subtitle, captions, axis titles, legend titles), discrete axis tick labels,
#' discrete legend keys, facet labels, and text from `geom_text`/`geom_label`.
#'
#' @param plot A ggplot object whose text elements are to be translated.
#' @param dictionary_list A named list where the names are the original text and the values are the translated text.
#' @return A modified ggplot object with all translatable text elements replaced
#'   according to the provided dictionary list.
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   day = factor(c("Monday", "Tuesday"), levels = c("Monday", "Tuesday")),
#'   value = c(10, 12)
#' )
#' translation_fr <- list(
#'   "Monday" = "Lundi",
#'   "Tuesday" = "Mardi",
#'   "Weekly Report" = "Rapport Hebdomadaire"
#' )
#' p_en <- ggplot(df, aes(x = day, y = value)) +
#'   geom_col() +
#'   labs(title = "Weekly Report")
#' p_fr <- ggtranslate(p_en, translation_fr)
#' \dontshow{
#' print(p_fr)
#' }
#' @import ggplot2 rlang
#' @importFrom stats setNames
#' @export
ggtranslate <- function(plot, dictionary_list) {
  # Create a true deep copy of the plot to avoid modifying the original object
  plot <- unserialize(serialize(plot, NULL))

  # Create a dictionary df with 2 columns
  dictionary <- data.frame(
    stringsAsFactors = FALSE,
    original = names(dictionary_list),
    translation = unlist(dictionary_list, use.names = FALSE)
  )

  # Create a named vector for easy lookup
  lookup <- setNames(dictionary$translation, dictionary$original)

  # Helper function to translate a vector of text
  translate_vector <- function(vec) {
    sapply(as.character(vec), function(t) ifelse(t %in% names(lookup), lookup[t], t), USE.NAMES = FALSE)
  }

  # Build the plot to ensure all scales and components are populated
  built_plot <- ggplot_build(plot)
  plot <- built_plot$plot


  ## 1. Translate main plot labels
  plot$labels <- lapply(plot$labels, function(label) {
    if (is.character(label)) translate_vector(label) else label
  })


  ## 2. Translate scales (axes and legends)
  for (i in seq_along(plot$scales$scales)) {
    scale <- plot$scales$scales[[i]]
    if (inherits(scale, "ScaleDiscrete")) {
      if (is.function(scale$labels)) {
        # If labels is already a function, wrap it to translate the output
        original_labels_func <- scale$labels
        scale$labels <- function(breaks) {
          translate_vector(original_labels_func(breaks))
        }
      } else if (is.character(scale$labels)) {
        # If labels are a character vector, translate it directly
        scale$labels <- translate_vector(scale$labels)
      } else {
        # This should handle NULL, waiver(), NA, etc.
        # The default behavior is to use breaks as labels.
        # So we create a function that translates the breaks.
        scale$labels <- function(breaks) {
          translate_vector(breaks)
        }
      }
    }
    plot$scales$scales[[i]] <- scale
  }

  ## 3. Translate facet labels
  if (!is.null(plot$facet) && inherits(plot$facet, "Facet")) {
    if (!is.null(plot$facet$params$labeller)) {
      original_labeller <- plot$facet$params$labeller
      plot$facet$params$labeller <- as_labeller(function(labels) {
        if (is.list(labels)) {
          lapply(labels, translate_vector)
        } else {
          translate_vector(labels)
        }
      })
    }
  }


  ## Tranlate data (doesnt work)
  # for (col in names(plot$data)) {
  #   if (is.character(plot$data[[col]])) {
  #     plot$data[[col]] <- translate_vector(plot$data[[col]]) ## DOESNT WORK, BREAKS PLOT
  #   }
  # }
  ## -> modifying plot$data breaks the whole plot in several ways


  ## 4. Translate text in geoms (geom_text, geom_label)
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    if (inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomLabel")) {
      if (!is.null(layer$aes_params$label)) {
        # a) Translate static labels (e.g. geom_text(label='static'))
        layer$aes_params$label <- translate_vector(layer$aes_params$label)
      } else {
        # b) Translate aesthetic-mapped labels (e.g. geom_text(aes(label=column)))
        label_var_name <- ""

        if ("label" %in% names(layer$mapping)) { ## this happens when aes is direclty defined: e.g. geom_text(aes(label=column))
          # in this case we will replace this value with a new column name which has translated values.
          label_var_name <- rlang::as_name(layer$mapping$label)
        } else if ("label" %in% names(layer$computed_mapping)) { ## this happens when aes is indireclty defined: e.g. ggplot(aes(label=column)) + geom_text(aes())
          # in this case we will insert a new layer$mapping, (value should be NULL)
          # this is not ideal, would be better to change the plot object as little as possible.
          # but this solution works.
          # we cannot change layer$computed_mapping (e.g. layer$computed_mapping$label <- rlang::sym(new_col_name))
          # because this value is overwritten each time we plot.
          label_var_name <- rlang::as_name(layer$computed_mapping$label)
        }

        if (label_var_name != "") {
          # find if the data is in the layer or the plot
          is_data_layer <- !is.null(layer$data) && !inherits(layer$data, "waiver")
          newdata <- if (layer_or_data) layer$data else plot$data

          if (!is.null(newdata) && label_var_name %in% names(newdata)) {
            ## add a column
            new_col_name <- paste0(label_var_name, "_translated")
            if (!new_col_name %in% names(newdata)) newdata[[new_col_name]] <- translate_vector(newdata[[label_var_name]])

            # replace the modified data back to the correct place
            if (is_data_layer) {
              layer$data <- newdata
            } else {
              plot$data <- newdata
            }

            # Update the aesthetic mapping to use the new translated column
            # note: layer$computed_mapping cannot be changed, its rewritten all the time
            layer$mapping$label <- rlang::sym(new_col_name)
          }
        }
      }
    }
    plot$layers[[i]] <- layer
  }


  ## todo
  # - factors ?


  plot
}
