#' Directory-style taxonomy tree axis guide
#'
#' @description
#' `guide_dirtree()` replaces an axis's flat text labels with an elbow tree of
#' the hierarchy behind them, in the style of the LaTeX `dirtree` package:
#' each break prints one node, indented by rank depth, joined to its parent by
#' a vertical trunk and a horizontal tick (`|_`). Identical paths collapse
#' into one node with a `× N` multiplier.
#'
#' @inheritParams ggplot2::guide_axis
#' @param levels Integer vector, same length as the scale's breaks: the 0-based
#'   rank depth of each break. Usually taken from [dirtree_data()]'s `level`.
#' @param n Integer vector, same length as the breaks: leaf counts per node;
#'   nodes with `n > 1` print ` × N` after the label. Default: all ones.
#' @param prefixes Character vector of rank prefixes, one per depth;
#'   `prefixes[level + 1]` is glued in front of each label. The default
#'   `c("p_", "c_", "o_", "f_", "g_", "s_")` matches phylum-through-species.
#'
#' @details
#' The breaks must arrive in depth-first tree order (parents before children,
#' siblings contiguous) — the order [dirtree_data()] returns. Wire it up with
#' `scale_*_discrete(breaks = ..., labels = ..., guide = "dirtree")` via
#' [guide_dirtree()], i.e. pass the guide to `guides()` or the scale's
#' `guide` argument.
#'
#' `angle`, `n.dodge`, `check.overlap` and `minor.ticks` from
#' [ggplot2::guide_axis()] are not supported: a tree label already occupies
#' the whole label band, so dodging and rotation are meaningless.
#'
#' @examples
#' library(ggplot2)
#' d <- dirtree_data(subset(tax_lineages, marker == "16S"))
#' ggplot(d, aes(x = label, y = n)) +
#'   geom_col() +
#'   scale_x_discrete(limits = d$label, breaks = d$label) +
#'   guides(x = guide_dirtree(levels = d$level, n = d$n))
#'
#' @family axis guides
#' @export
guide_dirtree <- function(title = ggplot2::waiver(), theme = NULL,
                          levels = NULL, n = NULL,
                          prefixes = c("p_", "c_", "o_", "f_", "g_", "s_"),
                          order = 0, position = ggplot2::waiver()) {
  if (!is.null(levels) && !(rlang::is_integerish(levels) && all(levels >= 0))) {
    rlang::abort("{.arg levels} must be a non-negative integer vector.")
  }
  if (!is.null(n) && !(rlang::is_integerish(n) && all(n >= 1))) {
    rlang::abort("{.arg n} must be a positive integer vector.")
  }
  if (!rlang::is_character(prefixes) || !length(prefixes)) {
    rlang::abort("{.arg prefixes} must be a non-empty character vector.")
  }
  # Local subclass carrying the custom params, so ggplot2's new_guide() keeps
  # them (it drops constructor args absent from super$params).
  super <- ggplot2::ggproto(
    "GuideDirtree", ggplot2::GuideAxis,
    params = c(
      ggplot2::GuideAxis$params,
      list(levels = NULL, n = NULL, prefixes = c("p_", "c_", "o_", "f_", "g_", "s_"))
    ),
    build_labels = function(key, elements, params) {
      if (".type" %in% names(key)) {
        key <- vctrs::vec_slice(key, key$.type == "major")
      }
      labels <- key$.label
      n_breaks <- length(labels)
      if (n_breaks < 1L) {
        return(list(grid::nullGrob()))
      }
      levels <- params$levels
      n <- params$n
      if (is.null(levels)) {
        rlang::abort(c(
          "{.fn guide_dirtree} needs {.arg levels}.",
          i = "One 0-based rank depth per break; see {.fn dirtree_data}."
        ))
      }
      .dirtree_check_lengths(levels, n, n_breaks)
      nodes <- .dirtree_nodes_from_levels(levels)
      nodes$n <- if (is.null(n)) rep(1L, n_breaks) else as.integer(n)
      # × N prints on leaf nodes only (gold standard: multiplier beside genera).
      leaf <- !(nodes$parent %in% seq_len(nrow(nodes)))
      text <- .dirtree_prefixed(labels, nodes$level, params$prefixes, nodes$n, leaf)
      list(dirtree_grob(nodes, text, gp = grid::gpar(
        col = elements$text$colour,
        fontsize = elements$text$size,
        fontface = elements$text$face,
        fontfamily = elements$text$family
      ), vertical = isTRUE(params$vertical)))
    }
  )
  ggplot2::new_guide(
    title = title, theme = theme,
    levels = if (is.null(levels)) NULL else as.integer(levels),
    n = if (is.null(n)) NULL else as.integer(n),
    prefixes = prefixes,
    available_aes = c("x", "y"),
    order = order, position = position,
    name = "dirtree", super = super
  )
}

.dirtree_check_lengths <- function(levels, n, n_breaks) {
  if (length(levels) != n_breaks) {
    rlang::abort(c(
      "{.arg levels} has length {length(levels)}, not the {n_breaks} break{?s} of the scale.",
      i = "Supply one level per break, in depth-first tree order."
    ))
  }
  if (!is.null(n) && length(n) != n_breaks) {
    rlang::abort("{.arg n} has length {length(n)}, not the {n_breaks} break{?s} of the scale.")
  }
}

# Minimal node table (label filled later) from a per-break level vector:
# parent of a node = nearest preceding row with level - 1 (depth-first,
# contiguous children).
.dirtree_nodes_from_levels <- function(levels) {
  levels <- as.integer(levels)
  parent <- integer(length(levels))
  for (i in seq_along(levels)) {
    parent[i] <- if (levels[i] == 0L) {
      NA_integer_
    } else {
      cand <- which(seq_len(i - 1L) < i & levels[seq_len(i - 1L)] == levels[i] - 1L)
      if (length(cand)) max(cand) else NA_integer_
    }
  }
  if (anyNA(parent) && any(levels > 0L)) {
    bad <- which(is.na(parent) & levels > 0L)[1]
    rlang::abort(c(
      "Break {bad} has rank depth {levels[bad]} with no parent at depth {levels[bad] - 1L} above it.",
      i = "Breaks must be in depth-first tree order (see {.fn dirtree_data})."
    ))
  }
  data.frame(label = NA_character_, level = levels, parent = parent,
             n = 1L, stringsAsFactors = FALSE)
}

# Strip a rank prefix from a display label (breaks are plotted on the prefixed
# node_key; the guide re-prefixes by node level), then add " × N" on leaves.
.dirtree_prefixed <- function(labels, levels, prefixes, n, leaf) {
  bare <- sub("^[pcofgs]_", "", labels)
  bare <- sub("^#[0-9]+_", "", bare)
  pre <- ifelse(levels < length(prefixes), prefixes[levels + 1L],
                paste0("#", levels, "_"))
  mult <- ifelse(leaf & n > 1L, paste0(" \u00d7 ", n), "")
  paste0(pre, bare, mult)
}
