# grid rendering of a dirtree layout (see R/tree.R for the geometry and its
# lineage to template/dirtree.dtx).
#
# The grob is a classed gTree ("dirtreeGrob") with registered
# heightDetails/widthDetails methods: a bare gTree measures 0, and ggplot2
# reserves the axis band from the measured size (plan round 2, MAJOR-B).
#
# Connector shape (dirtree.dtx:646-773, transcribed): a vertical trunk rule
# at the parent's rank column from the parent's baseline (+0.5 row for first
# children) to the farthest child; at each node's row a horizontal rule of
# 1 em, then 0.2 em gap, then the label. Roots hang off a free trunk at
# pitch/2. On the x-axis labels are rotated 90 deg (ladder rows are
# panel-width/40 at 40 breaks — horizontal labels collide); on the y-axis
# they are horizontal.

dirtree_grob <- function(nodes, labels, pos, gp = NULL, geom = NULL,
                        vertical = FALSE) {
  .check_dirtree_inputs(nodes, labels, pos)
  gp <- gp %||% grid::gpar()
  fontsize <- gp$fontsize %||% gp$size %||% 12
  geom <- geom %||% dirtree_geom(em_in = fontsize / 72.27)
  text_len <- nchar(as.character(labels)) * 0.55 * fontsize / 72.27
  ly <- dirtree_layout(nodes, pos, text_len = text_len, geom = geom)
  if (is.null(ly)) {
    return(grid::grid.null())
  }
  along <- function(a) grid::unit(as.numeric(a), "npc")
  # Depth coordinates run from the axis line into the band. For the x-axis
  # the axis line is the band TOP: grid y grows upward, so depth d maps to
  # 1npc - d. (For the y-axis, depth is along x from the band's left edge,
  # the axis line.) The band's measured size equals the tree's deepest
  # extent, so the 1npc expressions resolve against the band viewport.
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
  # Horizontal rule (dtx:763): width 1 em at the node's row, from its
  # trunk column; roots' rules run from the free trunk at pitch/2.
  hrule <- grid::segmentsGrob(
    x0 = if (vertical) depth_x(ly$tick$x0) else along(ly$y),
    y0 = if (vertical) along(ly$x)         else depth_y(ly$tick$x0),
    x1 = if (vertical) depth_x(ly$tick$x1) else along(ly$y),
    y1 = if (vertical) along(ly$x)         else depth_y(ly$tick$x1),
    gp = rule_gp(gp), name = "dirtree-rules"
  )
  txt <- grid::textGrob(
    label = labels,
    x = if (vertical) depth_x(ly$x_label) else along(ly$x),
    y = if (vertical) along(ly$x)         else depth_y(ly$x_label),
    # The label starts 1.2 em from its own trunk column and extends away
    # from the axis line (the gold model): anchored at its near end.
    hjust = 1,
    vjust = 0.5,
    rot = if (vertical) 0 else 90,
    gp = gp, name = "label"
  )
  grid::gTree(
    children = grid::gList(trunk, hrule,
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

#' Size report: the band's depth (perpendicular to the axis) is the tree's
#' extent (rank columns + rotated label length on the x-axis; rank columns on
#' the y-axis); the along-axis extent is the full cell (the panel span).
#' Consumed by ggplot2's measure_grobs via grid height/width generics.
#' Registered via @rawNamespace: `@export` on these methods emits NO
#' S3method line (roxygen cannot resolve the generic — grid is not
#' imported), measured in the post-exec sweep; the qualification is required.
#' @keywords internal
#' @noRd
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

#' Boundary validation for direct dirtree_grob() calls (the guide validates
#' the key/node alignment at its own slice site; this guards the ggplot-free
#' core: plan validation rule 4).
#' @keywords internal
#' @noRd
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

#' Rule gpar: thin butt-capped lines inheriting colour/linewidth from the
#' theme text, drawn under the labels.
#' @keywords internal
#' @noRd
rule_gp <- function(gp) {
  g <- grid::gpar(linewidth = 0.4, lineend = "butt", linejoin = "mitre")
  if (!is.null(gp)) {
    common <- intersect(names(gp), names(g))
    g[common] <- gp[common]
    g$col <- gp$col %||% gp$colour %||% g$col
  }
  g
}
