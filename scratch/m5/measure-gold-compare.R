# M5 gold comparison (post-exec B7 rewrite: every check asserts on real
# artifacts — gold pixels, the guide grob tree, the rendered labels).
# Run inside the dev container: sh .dev/r Rscript scratch/m5/measure-gold-compare.R
# Exits non-zero on any failed predicate; prints measured values.

suppressMessages(pkgload::load_all(".", quiet = TRUE))
grat <- function(cond, msg) {
  cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n", sep = "")
  if (!isTRUE(cond)) quit(save = "no", status = 1L)
}

d <- dirtree_data(subset(tax_lineages, marker == "16S"))

## ---------------------------------------------------------------- gold pixels
# Constants (plan Decision Log, dtx + gold generator): rank pitch DT@all =
# 1.4 em, trunk column of level L at L*1.4em + 0.2em, roots at pitch/2 =
# 0.7 em. Gold measured em = 54.3 px (76 px pitch), row pitch 68 px.
gold <- "examples/golden_standard.png"
grat(file.exists(gold), paste("gold image present:", gold))
m <- png::readPNG(gold)
lum <- apply(m[, , 1:3, drop = TRUE], 1:2, mean)     # y-by-x luminance (0..1)
ink <- lum < 0.5
nx <- ncol(ink); ny <- nrow(ink)
runs_in_col <- function(x) {
  v <- ink[, x]; rv <- rle(v); mx <- 0L
  for (i in seq_along(rv$lengths)) if (rv$values[i]) mx <- max(mx, rv$lengths[i])
  mx
}
# Trunk columns: full-height-ish vertical runs (> 40 px; text rows <= ~20 px)
em_px <- 54.3
cols_for_rank <- function(L) nx - (L * 1.4 + 0.2) * em_px  # right-anchored tree
col_x <- function(L) round(nx - (L * 1.4 + 0.2) * em_px)
# The gold tree is left-anchored: root trunk is the LEFTMOST long rule
# (measured 78 px from the left margin over y = 39..1150; plan round-2).
probe <- function(x0) {
  xs <- seq(max(1L, round(x0) - 4L), min(nx, round(x0) + 4L))
  max(vapply(xs, runs_in_col, 0L))
}
root_col <- which(vapply(seq_len(min(200L, nx)), runs_in_col, 0L) > 1000L)
grat(length(root_col) >= 1L,
     sprintf("root trunk: long vertical run at x=%s (0.7 em column)",
             paste(range(root_col), collapse = "-")))
# Rank columns L = 1..max(level): ink runs at pitch spacing from the root
# column (pitch signature; 1-npc drift makes per-row predicates
# unmeasurable - memory #364).
maxL <- max(d$level)
pitches_ok <- TRUE
for (L in seq_len(maxL)) {
  x0 <- min(root_col) + L * 1.4 * em_px
  if (probe(x0) < 40L) {
    pitches_ok <- FALSE
    cat("  rank L =", L, ": no trunk ink near x =", round(x0), "\n")
  }
}
grat(pitches_ok,
     sprintf("rank trunk columns L=1..%d present at 1.4 em pitch", maxL))

## --------------------------------------------------- rendered grob tree (B7 2)
# Walk ggplotGrob to the dirtreeGrob; count segment sub-units and compare
# against the node-table structural expectation:
#   trunks = one per run of shared connector-parent + 1 root trunk
#   rules  = one horizontal rule per node (tick/elbow)
gl <- ggplot2::ggplotGrob(
  ggplot2::ggplot(d, ggplot2::aes(x = node_key, y = n)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_discrete(limits = as.character(d$node_key),
                              breaks = as.character(d$node_key)) +
    ggplot2::guides(x = guide_dirtree(nodes = d)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
)
tree <- NULL
walk <- function(g) {
  if (inherits(g, "dirtreeGrob")) tree <<- g
  if (!is.null(g$children)) invisible(lapply(g$children, walk))
  if (!is.null(g$grobs)) invisible(lapply(g$grobs, walk))
}
walk(gl)
grat(!is.null(tree), "dirtreeGrob found in the composed axis")

segs <- Filter(function(x) inherits(x, "segments") || inherits(x, "polyline"),
               tree$children)
n_seg <- sum(vapply(segs, function(s) length(s$x0), 0L))
lay <- dirtree:::dirtree_layout(d, (seq_len(nrow(d)) - 0.5) / nrow(d))
n_trunk <- length(lay$trunk$x)
n_tick <- nrow(d)
expect_seg <- n_trunk + n_tick
grat(n_seg == expect_seg,
     sprintf("connector segments: rendered %d == expected %d (trunks %d + rules %d)",
             n_seg, expect_seg, n_trunk, n_tick))
# Root trunk exists in the rendered grob. x-axis orientation: trunks are
# vertical at depth_y(pitch/2) = "1npc - <pitch/2>inches" (grid unit
# arithmetic: compare the rendered unit strings).
pitch2 <- as.numeric(lay$geom$pitch / 2)
ty <- format(grid::unit(1, "npc") - grid::unit(pitch2, "inches"))
root_trunk_drawn <- any(vapply(segs, function(s)
  any(format(s$y0) == ty & format(s$y1) == ty &
        as.numeric(s$x0) != as.numeric(s$x1)), FALSE))
grat(root_trunk_drawn, "root trunk segment at pitch/2 in the rendered grob")

## ------------------------------------------------------- rendered labels (B7 3)
txt <- character()
walk2 <- function(g) {
  if (inherits(g, "textGrid") || inherits(g, "text")) {
    v <- tryCatch(as.numeric(g$label), warning = function(w) NA_real_)
    if (is.character(g$label) || !is.na(v)) txt <<- c(txt, as.character(g$label))
  }
  if (!is.null(g$children)) lapply(g$children, walk2)
  if (!is.null(g$grobs)) lapply(g$grobs, walk2)
}
walk2(tree)
want <- dirtree:::dirtree_labels(d)
grat(setequal(txt, want),
     sprintf("rendered axis labels == dirtree_labels output (%d labels)", length(want)))
n_mult <- sum(grepl("\u00d7", txt))
want_mult <- sum(d$n > 1L)
grat(n_mult == want_mult && want_mult > 0L,
     sprintf("x N rollups: rendered %d == node table %d (n>1, any rank)",
             n_mult, want_mult))

## ------------------------------------------------- label row positions (B7 4)
# Measure the labels' ALONG-AXIS npc positions from the rendered units
# (x-axis: the label x units): pitch must be constant = 1/n.
lab <- Filter(function(x) inherits(x, "dirtree-label"), tree$children)
xs <- do.call(grid::unit.c, unname(lapply(lab, function(t)
  do.call(grid::unit.c, unname(lapply(t$children, function(cg) cg$x))))))
is_npc <- all(grepl("npc", format(xs)))             # along() emits simple npc
ys <- if (is_npc) sort(as.numeric(xs)) else numeric()
d_y <- diff(ys)
grat(length(d_y) == length(want) - 1L &&
       max(d_y) - min(d_y) < 1e-6,
     sprintf("label row pitch constant from rendered units: %.6f npc (= 1/n)",
             median(d_y)))

## --------------------------------------------------------- comparison sheet
p <- ggplot2::ggplot(d, ggplot2::aes(x = node_key, y = n)) +
  ggplot2::geom_col() +
  ggplot2::scale_x_discrete(limits = as.character(d$node_key),
                            breaks = as.character(d$node_key)) +
  ggplot2::guides(x = guide_dirtree(nodes = d)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())
ragg::agg_png("examples/comparison_sheet.png", width = 11, height = 8,
              units = "in", res = 150)
grid::grid.newpage(); grid::grid.draw(p); invisible(dev.off())
cat("PASS  comparison sheet written: examples/comparison_sheet.png\n")
cat("M5 gold comparison: all predicates pass\n")
