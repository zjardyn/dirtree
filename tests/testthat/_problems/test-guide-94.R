# Extracted from test-guide.R:94

# prequel ----------------------------------------------------------------------
skip_if_not_installed("ggplot2")
plot_nodes <- function(d, breaks = d$node_key, guide = guide_dirtree(nodes = d)) {
  ggplot2::ggplot(d, ggplot2::aes(x = node_key, y = n)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_discrete(limits = as.character(breaks),
                              breaks = as.character(breaks)) +
    ggplot2::guides(x = guide) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}
rendered_text <- function(g, clamp = 500) {
  out <- character()
  walk <- function(x) {
    if (inherits(x, "textGrid") || inherits(x, "text")) {
      v <- tryCatch(as.numeric(x$label), warning = function(w) NA_real_)
      if (is.character(x$label) || !is.na(v)) out <<- c(out, as.character(x$label))
    }
    if (!is.null(x$children)) lapply(x$children, walk)
    if (!is.null(x$grobs)) lapply(x$grobs, walk)
  }
  walk(g)
  out
}

# test -------------------------------------------------------------------------
expect_error(guide_dirtree(), "must be given", fixed = TRUE)
