# Plain grob-tree traversal helpers (no display-list state).
find_child <- function(g, cls) {
  hits <- Filter(function(x) inherits(x, cls), g$children)
  hits
}

unit_values <- function(u) {
  if (inherits(u, "unit")) as.numeric(u) else u
}

test_that("dirtree_grob validates its inputs at the boundary", {
  d <- dirtree_data(toy_lineages())
  lab <- as.character(d$node_key)
  expect_error(dirtree_grob(d, lab[-1], pos = seq_len(nrow(d) - 1) / nrow(d)),
               "labels")
  expect_error(dirtree_grob(data.frame(x = 1), "a", pos = 0.5), "node table")
  expect_error(dirtree_grob(d, lab, pos = c(0.5)), "pos")
})

test_that("rows are placed at the supplied break positions", {
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  g <- dirtree_grob(d, as.character(d$node_key), pos = pos)
  labels <- do.call(c, lapply(find_child(g, "dirtree-label"), function(t) {
    unlist(lapply(t$children, function(cg) unit_values(cg$x)))
  }))
  expect_length(labels, nrow(d))
  expect_equal(as.numeric(labels), pos, tolerance = 1e-6)
})

test_that("grob reports real size (classed gTree + heightDetails)", {
  d <- dirtree_data(toy_lineages())
  g <- dirtree_grob(d, as.character(d$node_key),
                    pos = (seq_len(nrow(d)) - 0.5) / nrow(d))
  expect_s3_class(g, "dirtreeGrob")   # RED until cl is set
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = grid::unit(10, "cm"),
                                    height = grid::unit(20, "cm")))
  hh <- grid::convertHeight(grid::unit(1, "grobheight", data = list(g)), "cm",
                            valueOnly = TRUE)
  ww <- grid::convertWidth(grid::unit(1, "grobwidth", data = list(g)), "cm",
                           valueOnly = TRUE)
  expect_gt(hh, 0)   # RED: bare gTree measures 0
  expect_gt(ww, 0)
})

test_that("connector rules use the endpoint API and span the break range", {
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  g <- dirtree_grob(d, as.character(d$node_key), pos = pos)
  rules <- find_child(g, "segments")
  expect_gte(length(rules), 1L)
  expect_true(all(vapply(rules, function(s)
    all(c("x0", "y0", "x1", "y1") %in% names(s)), TRUE)))
  endpoints <- unlist(lapply(rules, function(s)
    list(s$x0, s$y0, s$x1, s$y1)), recursive = FALSE)
  npc <- vapply(endpoints, function(u) grepl("npc", format(u)[1]), TRUE)
  vals <- unlist(lapply(endpoints[npc], as.numeric))
  expect_gt(sum(npc), 0L)
  # all along-axis endpoints lie in the cell; trunks run between break rows
  # and the root trunk meets the axis line (npc 0), so:
  expect_true(all(vals >= 0 & vals <= 1))
  expect_gte(sum(vals %in% pos), 2L)
})
