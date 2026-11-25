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
  expect_s3_class(g, "dirtreeGrob")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = grid::unit(10, "cm"),
                                    height = grid::unit(20, "cm")))
  hh <- grid::convertHeight(grid::unit(1, "grobheight", data = list(g)), "cm",
                            valueOnly = TRUE)
  ww <- grid::convertWidth(grid::unit(1, "grobwidth", data = list(g)), "cm",
                           valueOnly = TRUE)
  expect_gt(hh, 0)
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
  # all along-axis endpoints lie in the cell
  expect_true(all(vals >= 0 & vals <= 1))
  expect_gte(sum(vals %in% pos), 2L)
})

test_that("rank columns are the exact dtx pitch on every row (M4/M5)", {
  # Gold rule: EVERY row's corner column (roots included) is
  # level*1.4em + 0.2em; the label starts 1.2em right of it. A width-driven
  # column model moves every one of these.
  d <- dirtree_data(toy_lineages())
  lay <- dirtree:::dirtree_layout(d, (seq_len(nrow(d)) - 0.5) / nrow(d))
  em <- lay$geom$em
  expect_equal(sort(unique(lay$x_conn)), em * c(0.2, 1.6, 3.0, 4.4, 5.8),
               tolerance = 1e-8)
  # labels sit on the AXIS side of their corner: x_label = x_conn - 1.2em
  expect_equal(sort(unique(lay$x_label)), em * c(-1, 0.4, 1.8, 3.2, 4.6),
               tolerance = 1e-8)
})

test_that("trunks hang from the parent's box bottom to the last arm (M6)", {
  # Gold: block trunk = children's corner column, parent centre - 0.44 row
  # (box bottom) down to last child's arm (centre - 0.21 row) - no stem.
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  lay <- dirtree:::dirtree_layout(d, pos, vertical = TRUE)
  em <- lay$geom$em
  n <- nrow(d)
  pitch <- 1 / n
  f1 <- which(d$label == "F1" & d$level == 3L)
  kids <- which(d$parent == f1)                      # G1, G2: the sibling pair
  blk <- which(abs(lay$trunk$x - 0.2 * em) < 1e-9)    # children's (G) column
  ends <- lapply(blk, function(b) sort(c(lay$trunk$y0[b], lay$trunk$y1[b])))
  f1_blk <- Filter(function(e) any(abs(e - (pos[f1] - 0.44 * pitch)) < 1e-8),
                   ends)
  expect_length(f1_blk, 1L)
  e <- f1_blk[[1]]
  expect_equal(e[1], pos[f1] - 0.44 * pitch, tolerance = 1e-8)
  expect_equal(e[2], pos[max(kids)] - 0.21 * pitch, tolerance = 1e-8)
})

test_that("root trunk spans first root's centre to the last root's arm", {
  d <- dirtree_data(toy_lineages())                  # two roots: rows 1 and 7
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  lay <- dirtree:::dirtree_layout(d, pos, vertical = TRUE)
  em <- lay$geom$em
  pitch <- 1 / nrow(d)
  roots <- which(d$level == 0L)
  expect_gte(length(roots), 2L)
  root_col <- max(lay$x_conn)                         # roots outermost
  is_root_trunk <- abs(lay$trunk$x - root_col) < 1e-9 &
    abs(lay$trunk$y0 - (pos[roots[1]] - 0.02 * pitch)) < 1e-8 &
    abs(lay$trunk$y1 - (pos[roots[length(roots)]] - 0.21 * pitch)) < 1e-8
  expect_true(any(is_root_trunk))
  # every root except the first row carries an arm on the root column
  arm_rows <- vapply(seq_along(roots), function(i) {
    r <- roots[i]
    any(abs(lay$tick$y0 - (pos[r] - 0.21 * pitch)) < 1e-8 &
          abs(lay$tick$x0 - root_col) < 1e-9 & lay$tick$show[r])
  }, TRUE)
  expect_equal(arm_rows, roots != 1L)
})

test_that("arms run from the node's own corner on the baseline row", {
  # Gold: the 1 em arm starts at the node's OWN corner column (root column
  # included) and lies on the text baseline = 0.21 row under the slot centre
  # (y-axis; centred rows on the rotated x-axis). Row 1 is unconnected.
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  em <- dirtree:::dirtree_geom()$em
  n <- nrow(d)
  pitch <- 1 / n
  x_conn_em <- (4L - d$level) * 1.4 + 0.2   # flipped: roots outermost
  ly_y <- dirtree:::dirtree_layout(d, pos, vertical = TRUE)
  ly_x <- dirtree:::dirtree_layout(d, pos, vertical = FALSE)
  expect_equal(ly_y$tick$x0, em * x_conn_em, tolerance = 1e-8)
  expect_equal(ly_y$tick$x1, em * (x_conn_em - 1), tolerance = 1e-8)
  expect_equal(ly_x$tick$x0, em * x_conn_em, tolerance = 1e-8)
  expect_equal(ly_x$tick$x1, em * (x_conn_em - 1), tolerance = 1e-8)
  expect_equal(ly_y$tick$y0, pos - 0.21 * pitch, tolerance = 1e-8)
  expect_equal(ly_x$tick$y0, pos, tolerance = 1e-8)
  expect_equal(ly_y$tick$show, seq_len(n) > 1L)
  # vectorization guard: arms at different ranks sit at different depths
  expect_gt(length(unique(round(ly_y$tick$x0 / em, 6))), 2L)
})

test_that("labels sit between tree and axis line and fit the band (B1)", {
  # hjust = 0 (anchor at the tree-side end, grow toward the axis line) +
  # band = max(x_conn) + sep + a shift that parks the longest label flush
  # with the axis line (the gold proportion: tree, labels, bars).
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  lab <- as.character(d$node_key)
  g <- dirtree_grob(d, lab, pos = pos, vertical = TRUE)
  txts <- unlist(lapply(find_child(g, "dirtree-label"),
                        function(t) t$children), recursive = FALSE)
  expect_length(txts, 1L)                            # one textGrob, all rows
  expect_equal(length(txts[[1L]]$label), nrow(d))
  expect_equal(as.numeric(txts[[1L]]$hjust), 0)
  tl <- nchar(lab) * 0.6 * 12 / 72.27
  lay <- dirtree:::dirtree_layout(d, pos, text_len = tl, vertical = TRUE)
  expect_equal(lay$band, max(lay$x_conn) + lay$geom$sep, tolerance = 1e-8)
  expect_equal(min(lay$x_label - tl), 0, tolerance = 1e-8)
})

test_that("the grob forwards `vertical`: x-axis arms are depth-only drops", {
  # Guards the grob's layout call: with vertical = FALSE the 1 em arms map
  # to depth-only segments (x0 == x1 along the axis) at the row positions.
  # Forcing the y-axis baseline offset (vertical = TRUE) shifts the row
  # endpoints off `pos` and fails.
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  g <- dirtree_grob(d, as.character(d$node_key), pos = pos, vertical = FALSE)
  hr <- Filter(function(x) identical(x$name, "dirtree-rules"), g$children)[[2]]
  x0 <- grid::convertX(hr$x0, "inches", valueOnly = TRUE)
  x1 <- grid::convertX(hr$x1, "inches", valueOnly = TRUE)
  y0 <- grid::convertY(hr$y0, "inches", valueOnly = TRUE)
  y1 <- grid::convertY(hr$y1, "inches", valueOnly = TRUE)
  lay <- dirtree:::dirtree_layout(d, pos, vertical = FALSE)
  em <- lay$geom$em
  expect_equal(length(x0), sum(lay$tick$show))       # row 1 unconnected
  expect_true(all(abs(x0 - x1) < 1e-6))              # depth-only arms
  expect_equal(sort(unique(round(abs(y0 - y1), 8))), rep(em, 1))  # 1 em spans
})
