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

test_that("rank columns are the exact dtx pitch, width-independent (M4/M5)", {
  # constants restored from dirtree.dtx: trunk column of a level-L node at
  # L*1.4em + 0.2em (roots on the free trunk at pitch/2); label 1.2em right
  # of its column. A width-driven column model moves every one of these.
  d <- dirtree_data(toy_lineages())
  lay <- dirtree:::dirtree_layout(d, (seq_len(nrow(d)) - 0.5) / nrow(d))
  em <- lay$geom$em
  # dtx:666: level-L column = L*1.4em + 0.2em; roots on the free trunk at
  # pitch/2 = 0.7em.
  expect_equal(sort(unique(lay$x_conn)), em * c(0.7, 1.6, 3.0, 4.4, 5.8),
               tolerance = 1e-8)
  expect_equal(sort(unique(lay$x_label)), em * c(1.9, 2.8, 4.2, 5.6, 7.0),
               tolerance = 1e-8)
})

test_that("trunks start at the parent row and extend 0.5 row past the far child (M6)", {
  # dtx:736-747: the rule runs parent baseline -> far child + 0.5 bl.
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  lay <- dirtree:::dirtree_layout(d, pos)
  em <- lay$geom$em
  n <- nrow(d)
  f1 <- which(d$label == "F1" & d$level == 3L)
  kids <- which(d$parent == f1)                     # G1, G2: the sibling pair
  blk <- which(abs(lay$trunk$x - 4.4 * em) < 1e-9)  # family-level trunks
  ends <- lapply(blk, function(b) sort(c(lay$trunk$y0[b], lay$trunk$y1[b])))
  f1_blk <- Filter(function(e) any(abs(e - pos[f1]) < 1e-8), ends)
  expect_length(f1_blk, 1L)
  e <- f1_blk[[1]]
  expect_equal(e[1], pos[f1], tolerance = 1e-8)      # starts at the parent
  # 0.5-row stem past the far child, in npc (rows are break positions)
  expect_equal(e[2], pos[max(kids)] + 0.5 / n, tolerance = 1e-8)
})

test_that("root trunk spans first..last root at pitch/2 (B4)", {
  d <- dirtree_data(toy_lineages())                 # two roots: rows 1 and 7
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  lay <- dirtree:::dirtree_layout(d, pos)
  em <- lay$geom$em
  roots <- which(d$level == 0L)
  expect_gte(length(roots), 2L)
  # the root block spans the first root -> last root + 0.5-row stem
  pitch_npc <- median(abs(diff(sort(pos))))
  is_root_trunk <- lay$trunk$x == em * 0.7 &
    pmin(lay$trunk$y0, lay$trunk$y1) == min(pos[roots]) &
    pmax(lay$trunk$y0, lay$trunk$y1) == max(pos[roots]) + 0.5 * pitch_npc
  expect_true(any(is_root_trunk))
  # every root row carries a tee (the horizontal rule into the free trunk)
  root_rows <- vapply(pos[roots], function(p)
    any(lay$tick$y0 == p & lay$tick$x0 == em * 0.7), TRUE)
  expect_true(all(root_rows))
})

test_that("connector legs land on the dtx corner on both axes (B1)", {
  # The leg's trunk end is the PARENT's column (roots: the free trunk at
  # 0.7em); the y-axis elbow runs to column + 1 em (roots: to the label
  # corner at column + 1.2 em); the x-axis leg drops to the label corner.
  # Regression: a scalar-`ifelse(vertical, ...)` recycled the ROOT's value
  # over every row, sending all legs to one column.
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  em <- dirtree:::dirtree_geom()$em
  cp <- as.integer(d$parent)
  x_conn_em <- ifelse(d$level == 0L, 0.7, d$level * 1.4 + 0.2)
  par_em <- ifelse(is.na(cp), 0.7, x_conn_em[ifelse(is.na(cp), 1L, cp)])
  ly_y <- dirtree:::dirtree_layout(d, pos, vertical = TRUE)
  ly_x <- dirtree:::dirtree_layout(d, pos, vertical = FALSE)
  expect_equal(ly_y$tick$x0, em * par_em, tolerance = 1e-8)
  expect_equal(ly_x$tick$x0, em * par_em, tolerance = 1e-8)
  expect_equal(ly_y$tick$x1,
               em * ifelse(d$level == 0L, x_conn_em + 1.2, x_conn_em + 1),
               tolerance = 1e-8)
  expect_equal(ly_x$tick$x1, em * (x_conn_em + 1.2), tolerance = 1e-8)
  # vectorization guard: legs at different ranks end at different depths
  expect_gt(length(unique(round(ly_y$tick$x1 / em, 6))), 2L)
})

test_that("labels are anchored at their column corner and grow away (B1)", {
  # hjust = 1 (anchor at the near end) + band = max(x_label + len) + sep:
  # the label occupies [column + 1.2 em, + len], never crossing shallower
  # trunks.
  d <- dirtree_data(toy_lineages())
  pos <- (seq_len(nrow(d)) - 0.5) / nrow(d)
  lab <- as.character(d$node_key)
  g <- dirtree_grob(d, lab, pos = pos, vertical = TRUE)
  txts <- unlist(lapply(find_child(g, "dirtree-label"),
                        function(t) t$children), recursive = FALSE)
  expect_length(txts, 1L)                            # one textGrob, all rows
  expect_equal(length(txts[[1L]]$label), nrow(d))
  expect_equal(as.numeric(txts[[1L]]$hjust), 1)
  tl <- nchar(lab) * 0.55 * 12 / 72.27
  lay <- dirtree:::dirtree_layout(d, pos, text_len = tl, vertical = TRUE)
  expect_equal(lay$band, max(lay$x_label + tl) + lay$geom$sep, tolerance = 1e-8)
})

test_that("the grob forwards `vertical`: x-axis legs drop to the label corner", {
  # Guards the grob's layout call: with vertical = FALSE the legs travel in
  # depth only (x0 == x1 along the axis) and span parent trunk -> label
  # corner, i.e. depth x1 - x0. Forcing the y-axis elbow here leaves the
  # legs horizontal and fails.
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
  roots <- which(d$level == 0L)
  expect_true(all(abs(x0 - x1) < 1e-6))                     # depth-only legs
  expect_equal(sort(unique(round(y0 - y1, 8))),
               sort(unique(round(lay$tick$x1 - lay$tick$x0, 8))))
  expect_equal(round(y0[roots] - y1[roots], 8), rep(1.2 * em, length(roots)))
})
