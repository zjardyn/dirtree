# S7-era ggplot2 guide: GuideAxis subclass driven by a node table.
#
# Contract (plan §Design): breaks are the node rows, plotted on `node_key`;
# the guide matches key labels to `nodes$node_key` by lookup (never
# positionally), places row i at the break position `key[[params$aes]]` (npc),
# and renders the tree via dirtree_grob(). Custom params ride in
# super$params — ggplot2::new_guide() drops constructor args absent from it.

#' Directory-style taxonomy tree as an axis guide
#'
#' `guide_dirtree()` replaces the plain text labels of a discrete position
#' scale with a directory-style tree (rank columns, `|_` connectors,
#' `p_`/`c_`/`o_`/`f_`/`g_` prefixes, ` × N` collapse counts on terminal
#' nodes). Build the node table with [dirtree_data()], plot on its
#' `node_key` column, and pass the same table here.
#'
#' @param nodes A node table from [dirtree_data()].
#' @param prefixes Rank prefixes by level; defaults
#'   `c("p_","c_","o_","f_","g_","s_")`. Display only.
#' @param title,theme,order,position Inherited from
#'   [ggplot2::guide_axis()]; `position` must be a horizontal or vertical
#'   axis (`"top"`/`"right"` secondary axes are not supported in v1).
#' @param angle,n.dodge,minor.ticks,cap Unsupported by the tree guide;
#'   supplying them (including `NULL`) errors.
#' @inheritParams ggplot2::guide_axis
#' @family axes
#' @examples
#' d <- dirtree_data(subset(tax_lineages, marker == "16S"))
#' ggplot(d, ggplot2::aes(x = node_key, y = n)) +
#'   ggplot2::geom_col() +
#'   ggplot2::scale_x_discrete(limits = d$node_key, breaks = d$node_key) +
#'   guides(x = guide_dirtree(nodes = d)) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::theme(axis.text.x = ggplot2::element_blank())
#' @export
guide_dirtree <- function(title = ggplot2::waiver(), theme = NULL,
                          nodes = NULL, prefixes = c("p_", "c_", "o_", "f_", "g_", "s_"),
                          angle = NULL, n.dodge = NULL, minor.ticks = NULL,
                          cap = NULL, order = 0, position = ggplot2::waiver()) {
  for (nm in c("angle", "n.dodge", "minor.ticks", "cap")) {
    if (!is.null(get(nm))) {
      .dt_abort(c(
        paste0("{.fn guide_dirtree} does not support {.arg ", nm, "}."),
        i = "The tree lays out its own ranks; see {.fn dirtree_geom}-backed args in {.fn dirtree_grob}."
      ))
    }
  }
  if (is.null(nodes)) {
    .dt_abort(c(
      "{.fn guide_dirtree} needs {.arg nodes}.",
      i = "Build a node table with {.fn dirtree_data}."
    ))
  }
  .check_dirtree_node_table(nodes)
  super <- ggplot2::ggproto(
    "GuideDirtree", ggplot2::GuideAxis,
    params = c(
      ggplot2::GuideAxis$params,
      list(nodes = NULL, prefixes = prefixes)
    ),
    build_labels = function(key, elements, params) {
      if (".type" %in% names(key)) {
        key <- vctrs::vec_slice(key, key$.type == "major")
      }
      labels <- as.character(key$.label)
      if (length(labels) < 1L) {
        return(list(grid::nullGrob()))
      }
      nodes <- params$nodes
      idx <- match(labels, as.character(nodes$node_key))
      unmatched <- labels[is.na(idx)]
      if (length(unmatched)) {
        .dt_abort(c(
          "{.fn guide_dirtree} found break{?s} missing from {.arg nodes}: {.val {unmatched}}.",
          i = "Plot breaks on {.field node_key} and pass the full node table from {.fn dirtree_data}."
        ))
      }
      nd <- nodes[idx, , drop = FALSE]
      newpar <- match(nodes$parent[idx], idx)
      orphan <- which(is.na(newpar) & nd$level > 0L)
      if (length(orphan)) {
        .dt_abort(c(
          "{.fn guide_dirtree} breaks are not tree-closed: {.val {labels[orphan]}} has no parent row.",
          i = "Include every ancestor rank in {.field breaks}."
        ))
      }
      nd$parent <- as.integer(newpar)
      leaf <- !(seq_len(nrow(nd)) %in% nd$parent[!is.na(nd$parent)])
      text <- .dirtree_prefixed(labels, nd$level, params$prefixes, nd$n, leaf)
      gp <- grid::gpar(
        col = elements$text$colour,
        fontsize = elements$text$size,
        fontface = elements$text$face,
        fontfamily = elements$text$family
      )
      list(dirtree_grob(
        nd, text,
        pos = key[[params$aes]],
        gp = gp,
        vertical = isTRUE(params$vertical)
      ))
    }
  )
  ggplot2::new_guide(
    title = title, theme = theme,
    nodes = nodes, prefixes = prefixes,
    available_aes = c("x", "y"),
    order = order, position = position,
    name = "dirtree", super = super
  )
}

# Node table shape (plan validation rule 1).
.check_dirtree_node_table <- function(nodes) {
  need <- c("label", "level", "parent", "n", "node_key")
  if (!is.data.frame(nodes) || !all(need %in% names(nodes))) {
    .dt_abort(c(
      "{.arg nodes} must be a node table with columns {.val {need}}.",
      i = "Build one with {.fn dirtree_data}."
    ))
  }
  if (!rlang::is_integerish(nodes$level) || any(nodes$level < 0)) {
    .dt_abort("{.field level} must be 0-based integer rank depths.")
  }
  levels <- as.integer(nodes$level)
  if (length(levels) && levels[1] != 0L) {
    .dt_abort(c(
      "{.arg nodes} is not depth-first: the first row has rank depth {levels[1]}, not 0.",
      i = "Rows must visit every parent before its children (see {.fn dirtree_data})."
    ))
  }
  bad <- vapply(seq_along(levels), function(i) {
    if (levels[i] == 0L) {
      !is.na(nodes$parent[i])
    } else {
      p <- nodes$parent[i]
      is.na(p) || p >= i || levels[p] != levels[i] - 1L
    }
  }, logical(1))
  if (any(bad)) {
    .dt_abort(c(
      "{.arg nodes} row(s) {which(bad)} violate the depth-first contract (parent must precede at rank depth level - 1).",
      i = "Build the table with {.fn dirtree_data}."
    ))
  }
  if (anyDuplicated(nodes$node_key)) {
    .dt_abort("{.field node_key} must be unique.")
  }
  invisible(TRUE)
}

# Strip a rank prefix from a break label (breaks are plotted on the prefixed
# node_key), re-prefix by node level, add " × N" on terminal nodes.
.dirtree_prefixed <- function(labels, levels, prefixes, n, leaf) {
  bare <- sub("^[pcofgs]_", "", labels)
  bare <- sub("^#[0-9]+_", "", bare)
  pre <- ifelse(levels < length(prefixes), prefixes[levels + 1L],
                paste0("#", levels, "_"))
  mult <- ifelse(leaf & n > 1L, paste0(" \u00d7 ", n), "")
  paste0(pre, bare, mult)
}

# Validate a bare depth-first level vector and derive parents (used by
# dirtree_data() and tests; plan validation rule 2 - the abort requires a
# NON-EMPTY bad index: valid trees like c(0,1,2,3,4,0,1,2,3,4) must pass).
.dirtree_nodes_from_levels <- function(levels) {
  levels <- as.integer(levels)
  parent <- vapply(seq_along(levels), function(i) {
    if (levels[i] == 0L) {
      NA_integer_
    } else {
      cand <- which(levels[seq_len(i - 1L)] == levels[i] - 1L)
      if (length(cand) == 0L) NA_integer_ else max(cand)
    }
  }, integer(1))
  bad <- which(is.na(parent) & levels > 0L)
  if (length(bad) > 0L) {
    .dt_abort(c(
      "Break {bad[1]} has rank depth {levels[bad[1]]} with no parent at depth {levels[bad[1]] - 1L} above it.",
      i = "Breaks must be in depth-first tree order (see {.fn dirtree_data})."
    ))
  }
  vctrs::data_frame(label = NA_character_, level = levels, parent = parent)
}
