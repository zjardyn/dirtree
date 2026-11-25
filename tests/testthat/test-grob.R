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
