# grid rendering of a dirtree layout (see R/tree.R for the geometry and its
# lineage to template/dirtree.dtx).
#
# The grob is a classed gTree ("dirtreeGrob") with registered
# heightDetails/widthDetails methods: a bare gTree measures 0, and ggplot2
# reserves the axis label row from the measured size (plan reviewer #2).
#
# Connector shape (gold standard): a trunk rule runs along the axis at the
# parent's rank column from the parent's row to the parent's farthest child;
# each child gets an elbow at its own row into its label. On the x-axis
# labels are rotated 90 deg (ladder rows are panel-width/40 at 40 breaks --
# horizontal labels collide); on the y-axis they are horizontal.

dirtree_grob <- function(nodes, labels, pos, gp = NULL, geom = NULL,
                        vertical = FALSE) {
  .check_dirtree_inputs(nodes, labels, pos)
  gp <- gp %||% grid::gpar()
  fontsize <- gp$fontsize %||% gp$size %||% 12
  geom <- geom %||% dirtree_geom(em_in = fontsize / 72.27)
  widths <- .dirtree_col_widths(labels, nodes$level, gp, geom)
  text_len <- nchar(as.character(labels)) * 0.55 * fontsize / 72.27
  ly <- dirtree_layout(nodes, pos, widths, text_len, geom)
  if (is.null(ly)) {
    return(grid::grid.null())
  }
  along <- function(a) grid::unit(as.numeric(a), "npc")
  # Depth coordinates run from the axis line into the band. For the x-axis
  # the axis line is the band TOP: grid y grows upward, so depth d maps to
  # 1npc - d. (For the y-axis, depth is along x from the band's right edge.)
  # The band's measured height equals the tree's deepest extent, so no
  # viewport is needed: the 1npc expression resolves against the band.
  depth_x <- function(d) {
    if (vertical) grid::unit(1, "npc") - grid::unit(d, "inches")
    else grid::unit(d, "inches")
  }
  depth_y <- function(d) grid::unit(1, "npc") - grid::unit(d, "inches")
  trunk <- grid::segmentsGrob(
    x0 = if (vertical) depth_x(ly$trunk$x) else along(ly$trunk$y0),
    y0 = if (vertical) along(ly$trunk$y0)    else depth_y(ly$trunk$x),
    x1 = if (vertical) depth_x(ly$trunk$x) else along(ly$trunk$y1),
    y1 = if (vertical) along(ly$trunk$y1)    else depth_y(ly$trunk$x),
    gp = rule_gp(gp), name = "dirtree-rules"
  )
  # Elbow: at each node's row, from its trunk column into the label anchor.
  elbow <- grid::segmentsGrob(
    x0 = if (vertical) depth_x(ly$x_trunk) else along(ly$y),
    y0 = if (vertical) along(ly$x)         else depth_y(ly$x_trunk),
    x1 = if (vertical) depth_x(ly$x_label) else along(ly$y),
    y1 = if (vertical) along(ly$x)         else depth_y(ly$x_label),
    gp = rule_gp(gp), name = "dirtree-rules"
  )
  txt <- grid::textGrob(
    label = labels,
    x = if (vertical) depth_x(ly$x_label) else along(ly$x),
    y = if (vertical) along(ly$x)         else depth_y(ly$x_label),
    hjust = if (vertical) 1 else 1,
    vjust = 0.5,
    rot = if (vertical) 0 else 90,
    gp = gp, name = "label"
  )
  grid::gTree(
    children = grid::gList(trunk, elbow,
                           grid::gTree(children = grid::gList(txt),
                                       name = "dirtree-label",
                                       cl = "dirtree-label")),
    vertical = vertical,
    depth = ly$depth,
    band = ly$band,
    geom = geom,
    vp = grid::viewport(name = "dirtree"),
    cl = "dirtreeGrob"
  )
}

# Size report: the band's depth (perpendicular to the axis) is the tree's
# extent (rank columns + rotated label length on the x-axis; rank columns on
# the y-axis); the along-axis extent is the full cell (the panel span).
# Consumed by ggplot2's measure_grobs via grid height/width generics.
#' @rawNamespace S3method(grid::heightDetails, dirtreeGrob)
#' @rawNamespace S3method(grid::widthDetails, dirtreeGrob)
heightDetails.dirtreeGrob <- function(x) {
  if (isTRUE(x$vertical)) grid::unit(1, "npc")
  else grid::unit(x$band, "inches")
}
widthDetails.dirtreeGrob <- function(x) {
  if (isTRUE(x$vertical)) grid::unit(x$band, "inches")
  else grid::unit(1, "npc")
}

# Boundary validation for direct dirtree_grob() calls (the guide validates
# the key/node alignment at its own slice site; this guards the ggplot-free
# core: plan validation rule 4).
.check_dirtree_inputs <- function(nodes, labels, pos) {
  need <- c("label", "level", "parent", "n")
  if (!is.data.frame(nodes) || !all(need %in% names(nodes))) {
    .dt_abort(c(
      "{.arg nodes} must be a node table with columns {.val {need}}.",
      i = "Build one with {.fn dirtree_data}."
    ))
  }
  if (length(labels) != nrow(nodes)) {
    .dt_abort(
      "{.arg labels} has length [{length(labels)}], not the [{nrow(nodes)}] node rows."
    )
  }
  if (length(pos) != nrow(nodes)) {
    .dt_abort(
      "{.arg pos} has length [{length(pos)}], not the [{nrow(nodes)}] node rows."
    )
  }
  invisible(TRUE)
}

rule_gp <- function(gp) {
  g <- grid::gpar(linewidth = 0.4, lineend = "butt", linejoin = "mitre")
  if (!is.null(gp)) {
    common <- intersect(names(gp), names(g))
    g[common] <- gp[common]
    g$col <- gp$col %||% gp$colour %||% g$col
  }
  g
}

# Label-column width per rank level (inches): the widest prefixed label at
# that level plus the connector gutters. Device-free estimate (0.55 em per
# character): stringWidth needs a real device and returns 0/NA on the NULL
# device used by R CMD check, and the pure-data geometry stays grid-free.
.dirtree_col_widths <- function(labels, levels, gp, geom) {
  w <- nchar(as.character(labels)) * 0.55 * (gp$fontsize %||% 12) / 72.27
  lvl <- as.integer(levels)
  out <- rep(geom$pitch, max(lvl) + 1L)
  for (l in seq_along(out)) {
    i <- which(lvl == l - 1L)
    if (length(i)) {
      out[l] <- max(w[i]) + 0.4 * geom$em
    }
  }
  out
}
