# grid rendering of a dirtree layout (see R/tree.R for the geometry and its
# lineage to template/dirtree.dtx).
#
# The grob is a classed gTree ("dirtreeGrob") with registered
# heightDetails/widthDetails methods: a bare gTree measures 0, and ggplot2
# reserves the axis band from the measured size (plan round 2, MAJOR-B).
#
# Connector shape (dirtree.dtx:646-773, transcribed) is axis-aware: a trunk
# rule runs along the parent's rank column across its children's block. At
# each node the leg into the label turns the dtx corner at the node's own
# trunk column: on the y-axis a horizontal rule from the parent's trunk
# across 1 em, then 0.2 em gap, then the label (the dtx elbow); on the
# x-axis the tree is rotated 90 deg and the leg drops vertically from the
# (horizontal) parent trunk into the label corner. Roots hang off a free
# trunk at pitch/2. On the x-axis labels are rotated 90 deg (ladder rows are
# panel-width/40 at 40 breaks — horizontal labels collide); on the y-axis
# they are horizontal.

dirtree_grob <- function(nodes, labels, pos, gp = NULL, geom = NULL,
                        vertical = FALSE) {
  .check_dirtree_inputs(nodes, labels, pos)
  gp <- gp %||% grid::gpar()
  fontsize <- gp$fontsize %||% gp$size %||% 12
  geom <- geom %||% dirtree_geom(em_in = fontsize / 72.27)
  text_len <- nchar(as.character(labels)) * 0.55 * fontsize / 72.27
  ly <- dirtree_layout(nodes, pos, text_len = text_len, geom = geom,
                       vertical = vertical)
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
  # Axis-aware placement: a point (row r, depth d). Rows keep the break
  # order (r = break npc); the x-axis grows the tree downward from the band
  # top, the y-axis leftward from the axis line.
  px <- function(r, d) if (vertical) depth_x(d) else along(r)
  py <- function(r, d) if (vertical) along(r)   else depth_y(d)
  trunk <- grid::segmentsGrob(
    x0 = px(ly$trunk$y0, ly$trunk$x),
    y0 = py(ly$trunk$y0, ly$trunk$x),
    x1 = px(ly$trunk$y1, ly$trunk$x),
    y1 = py(ly$trunk$y1, ly$trunk$x),
    gp = rule_gp(gp), name = "dirtree-rules"
  )
  # Leg into each node's label (dtx:763-764): the y-axis elbow is the
  # horizontal 1 em rule from the trunk column; on the rotated x-axis the
  # leg drops from the trunk depth into the label corner (x1 == x0 in the
  # layout keeps the drop perpendicular to the horizontal trunks).
  hrule <- grid::segmentsGrob(
    x0 = px(ly$tick$y0, ly$tick$x0),
    y0 = py(ly$tick$y0, ly$tick$x0),
    x1 = px(ly$tick$y1, ly$tick$x1),
    y1 = py(ly$tick$y1, ly$tick$x1),
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
