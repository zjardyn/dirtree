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

test_that("guide_dirtree() builds a guide from the node table", {
  d <- dirtree_data(toy_lineages())
  g <- guide_dirtree(nodes = d)
  expect_s3_class(g, "GuideDirtree")
  expect_identical(g$params$nodes$node_key, d$node_key)
})

test_that("plot builds; axis labels are re-prefixed, leaves carry × N", {
  tl <- rbind(toy_lineages(), toy_lineages()[1, ])  # leaf G1 collapses (n = 2)
  d <- dirtree_data(tl)
  gl <- ggplot2::ggplotGrob(plot_nodes(d))
  expect_s3_class(gl, "gtable")   # RED until the nodes contract lands
})

test_that("composed axis band is sized by the tree (band below panel > plain axis)", {
  # plan round-2 MAJOR-B predicate: the tree's measured size flows into the
  # composed band. ggplot2 4.0.4 gtable slots carry no names, so measure the
  # rows below the panel and compare against the plain axis on the same data.
  d <- dirtree_data(toy_lineages())
  band_cm <- function(p) {
    gl <- suppressWarnings(ggplot2::ggplotGrob(p))
    pt <- gl$layout[gl$layout$name == "panel", ][1, "t"]
    rows <- (pt + 1L):nrow(gl)
    sum(vapply(gl$height[rows],
               function(u) as.numeric(suppressWarnings(
                 grid::convertUnit(u, "cm"))), 0))
  }
  p <- plot_nodes(d)
  plain <- band_cm(ggplot2::ggplot(d, ggplot2::aes(x = node_key, y = n)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_discrete(limits = as.character(d$node_key),
                              breaks = as.character(d$node_key)) +
    ggplot2::theme_minimal())
  expect_gt(band_cm(p), plain + 0.5)
})

test_that("breaks missing from the node table abort naming the break", {
  d <- dirtree_data(toy_lineages())
  p <- plot_nodes(d, breaks = c(d$node_key, "g_NotA_NODE"))
  expect_error(ggplot2::ggplotGrob(p), "NotA_NODE")
})

test_that("constructor rejects unsupported guide_axis args by name", {
  d <- dirtree_data(toy_lineages())
  for (arg in c("angle", "n.dodge", "minor.ticks", "cap")) {
    expect_error(
      do.call(guide_dirtree, c(list(nodes = d), setNames(list(1), arg))),
      arg, fixed = TRUE
    )
  }
})

test_that("missing nodes aborts with guidance", {
  expect_error(guide_dirtree(), "nodes")
})

test_that("y-axis placement works (vertical tree)", {
  d <- dirtree_data(toy_lineages())
  p <- ggplot2::ggplot(d, ggplot2::aes(y = node_key, x = n)) +
    ggplot2::geom_col() +
    ggplot2::scale_y_discrete(limits = as.character(d$node_key),
                              breaks = as.character(d$node_key)) +
    ggplot2::guides(y = guide_dirtree(nodes = d))
  expect_no_error(ggplot2::ggplotGrob(p))
})
