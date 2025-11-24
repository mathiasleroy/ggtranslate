#' Translate all text elements in a ggplot2 object
#'
#' This function takes a ggplot2 object and a named list to translate
#' all user-facing text elements within the plot. This includes main plot labels
#' (title, subtitle, captions, axis titles, legend titles), discrete axis tick labels,
#' discrete legend keys, facet labels, and text from `geom_text`/`geom_label`.
#'
#' @param plot A ggplot object whose text elements are to be translated.
#' @param dictionary_list A named list (or named character vector) where the names are the original text and the values are the translated text.
#' @param mode Translation mode:
#'   `"strict"` (default, exact match only),
#'   `"longfirst"` (partial replacement, longest key first), or
#'   `"asordered"` (partial replacement, as ordered in the dict).
#' @return A modified ggplot object with all translatable text elements replaced
#'   according to the provided dictionary list.
#'
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
#'   "day" = "jour",
#'   "value" = "valeur",
#'   "Weekly Report" = "Rapport Hebdomadaire"
#' )
#' p_en <- ggplot(df, aes(x = day, y = value)) +
#'   geom_col() +
#'   labs(title = "Weekly Report")
#' p_fr <- ggtranslate(p_en, translation_fr)
#' \dontshow{
#' print(p_fr)
#' }
#' @importFrom ggplot2 ggplot_build as_labeller
#' @importFrom rlang quo_get_expr is_symbol as_name is_call sym eval_tidy
#' @importFrom stats setNames
#' @export
ggtranslate <- function(plot,
                        dictionary_list,
                        mode = "strict" # "strict", "longfirst", "asordered"
) {
  # order dicionay by  longest keys first (avoids str matches within strings)
  reorder_by_key_length <- function(x, decreasing = TRUE) {
    if (is.null(names(x))) stop("Input must be a named list")
    x[order(nchar(names(x)), decreasing = decreasing)]
  }
  if (mode == "longfirst") {
    dictionary_list <- reorder_by_key_length(dictionary_list)
  }

  # transform to named character vector
  lookup <- setNames(unlist(dictionary_list, use.names = FALSE), names(dictionary_list))

  # Helper function to translate bits within a string
  translate_string <- function(text) {
    for (orig in names(lookup)) {
      text <- gsub(orig, lookup[orig], text, fixed = TRUE)
    }
    text
  }

  # Helper function to vectorize translate_string
  if (mode %in% c("longfirst", "asordered")) {
    # partial replacements, as ordered in lookup
    translate_vector <- function(vec) {
      sapply(as.character(vec), translate_string, USE.NAMES = FALSE)
    }
  } else { # mode == "strict" (or any non-valid mode)
    # this version will only translate exact complete strings in the dictionary
    translate_vector <- function(vec) {
      sapply(as.character(vec), function(t) ifelse(t %in% names(lookup), lookup[t], t), USE.NAMES = FALSE)
    }
  }


  # Create a true deep copy of the plot to avoid modifying the original object
  plot <- unserialize(serialize(plot, NULL))

  # Build the plot to ensure all scales and components are populated
  built_plot <- ggplot_build(plot)
  plot <- built_plot$plot


  ## 1. Translate main plot labels
  for (label_name in names(plot$labels)) {
    if (is.character(plot$labels[[label_name]])) {
      plot$labels[[label_name]] <- translate_vector(plot$labels[[label_name]])
    }
  }


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

        # find if the data is in the layer or the plot / FALSE if plot
        is_data_in_layer <- !is.null(layer$data) && !inherits(layer$data, "waiver")

        if ("label" %in% names(layer$mapping)) { ## this happens when aes is direclty defined: e.g. geom_text(aes(label=column))
          # --> we will replace this mapping with a new column
          mapping_or_computed <- "mapping"
        } else if ("label" %in% names(layer$computed_mapping)) { ## this happens when aes is indireclty defined: e.g. ggplot(aes(label=column)) + geom_text(aes())
          # --> we will replace layer$mapping with a new column
          # (we cannot change layer$computed_mapping, its overwritten)
          mapping_or_computed <- "computed_mapping"
        }

        label_quosure <- layer[[mapping_or_computed]]$label
        label_expr <- rlang::quo_get_expr(label_quosure)
        label_var_name <- NULL # Initialize to NULL
        if (rlang::is_symbol(label_expr)) {
          # If the label is a simple symbol (e.g., `var_name`), get its name
          # e.g.  > layer[[mapping_or_computed]]
          label_var_name <- rlang::as_name(label_expr)
        } else if (rlang::is_call(label_expr)) {
          # If the label is an expression (e.g., `abs(count)` or `paste(name1, name2)`)
          # e.g. `label` -> `abs(count)`
          # e.g. `label` -> `paste(name1, name2, sep = " - ")`

          ## add new column
          # label_var_name <- paste0(".ggtranslate_label_temp_", digest::digest(label_expr)) ## avoid library
          label_var_name <- paste(deparse(label_expr, width.cutoff = 500L), collapse = "")

          if (is_data_in_layer) {
            layer$data[[label_var_name]] <- rlang::eval_tidy(label_quosure, layer$data)
          } else {
            plot$data[[label_var_name]] <- rlang::eval_tidy(label_quosure, plot$data)
          }
        }

        if (is.null(label_var_name)) next
        if (label_var_name == "") next

        ## find data type
        label_var_type <- ifelse(is_data_in_layer,
          class(layer$data[[label_var_name]]),
          class(plot$data[[label_var_name]])
        )
        if (label_var_type == "numeric") next ## dont translate numeric

        # Create new data frame for adding translation column
        newdata <- if (is_data_in_layer) layer$data else plot$data

        if (!is.null(newdata) && label_var_name %in% names(newdata)) {
          ## add a column
          new_col_name <- paste0(label_var_name, "_translated")
          if (!new_col_name %in% names(newdata)) newdata[[new_col_name]] <- translate_vector(newdata[[label_var_name]])

          # replace the modified data back to the correct place
          if (is_data_in_layer) {
            layer$data <- newdata
          } else {
            plot$data <- newdata
          }

          # Update the aesthetic mapping
          # note: layer$computed_mapping cannot be changed, its overwritten
          layer$mapping$label <- rlang::sym(new_col_name)
        }
      }
    }
    plot$layers[[i]] <- layer
  }


  ## todo
  # - factors ?


  plot
}
