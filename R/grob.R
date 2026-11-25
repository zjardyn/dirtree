# grid rendering of a dirtree layout (see R/tree.R for the geometry and its
# lineage to template/dirtree.dtx).
#
# The grob is a classed gTree ("dirtreeGrob") with registered
# heightDetails/widthDetails methods: a bare gTree measures 0, and ggplot2
# reserves the axis band from the measured size (plan round 2, MAJOR-B).
#
# Connector shape (gold-pixel model, see R/tree.R) is axis-aware: a block's
# trunk hangs from the parent's box bottom down the children's corner column
# to the last child's arm; each node (except the first row) gets a horizontal
# 1 em arm on its text baseline from its own corner column TOWARD the axis
# line, then the 0.2 em gap, then the label growing toward the axis line
# (tree outermost, labels between tree and panel, as in the gold). Roots
# share their own corner column as a trunk spanning first root..last root's
# arm. On the x-axis the tree is rotated 90 deg through the same coordinate
# mapping; labels are rotated 90 deg there (ladder rows are panel-width/40
# at 40 breaks — horizontal labels collide).

dirtree_grob <- function(nodes, labels, pos, gp = NULL, geom = NULL,
                        vertical = FALSE) {
  .check_dirtree_inputs(nodes, labels, pos)
  gp <- gp %||% grid::gpar()
  fontsize <- gp$fontsize %||% gp$size %||% 12
  geom <- geom %||% dirtree_geom(em_in = fontsize / 72.27)
  # Gold typography: typewriter labels, genus/species NAMES italic
  # (\texttt{g\_}{\itshape ...}). Courier advance width is exactly 0.6 em,
  # so text_len is exact, not a heuristic.
  face <- ifelse(grepl("^[gs]_", labels), "italic", "plain")
  gp <- grid::gpar(
    fontfamily = "mono", fontface = face, fontsize = fontsize,
    col = gp$col %||% grid::gpar("col")$col
  )
  text_len <- nchar(as.character(labels)) * 0.6 * fontsize / 72.27
  ly <- dirtree_layout(nodes, pos, text_len = text_len, geom = geom,
                       vertical = vertical)
  if (is.null(ly)) {
    return(grid::grid.null())
  }
  along <- function(a) grid::unit(as.numeric(a), "npc")
  # Depth coordinates run from the axis line into the band. For the x-axis
  # the axis line is the band TOP: grid y grows upward, so depth d maps to
  # 1npc - d. (For the y-axis — a left axis — the axis line is the band's
  # RIGHT edge, so depth runs from 1npc leftward.) The band's measured size
  # equals the tree's deepest extent, so the 1npc expressions resolve
  # against the band viewport.
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
    gp = rule_gp(gp, ly$geom$rule), name = "dirtree-rules"
  )
  # Connector arm into each node's label (gold / dtx:757-768): the 1 em rule
  # from the node's own corner column toward the label. Layout's tick `x*`
  # are DEPTHS from the axis line; px()/py() map a (row, depth) point to the
  # axis frame, so on the x-axis the arm becomes the vertical drop onto the
  # label column. The first row (first root) is unconnected; layout marks it
  # show = FALSE.
  show <- ly$tick$show
  hrule <- grid::segmentsGrob(
    x0 = px(ly$tick$y0[show], ly$tick$x0[show]),
    y0 = py(ly$tick$y0[show], ly$tick$x0[show]),
    x1 = px(ly$tick$y1[show], ly$tick$x1[show]),
    y1 = py(ly$tick$y1[show], ly$tick$x1[show]),
    gp = rule_gp(gp, ly$geom$rule), name = "dirtree-rules"
  )
  txt <- grid::textGrob(
    label = labels,
    x = if (vertical) depth_x(ly$x_label) else along(ly$x),
    y = if (vertical) along(ly$x)         else depth_y(ly$x_label),
    # The label starts 1.2 em from its own trunk column on the AXIS side and
    # grows toward the axis line (the gold model: tree outermost, labels
    # between the tree and the panel).
    hjust = 0,
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
#' rank-column extent plus the outward shift that parks the longest label
#' flush with the axis line (the labels lie INSIDE that span — the gold's
#' proportion: tree, labels, bars); the along-axis extent is the full cell
#' (the panel span).
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

#' Rule gpar: thin butt-capped lines inheriting colour from the theme text,
#' drawn under the labels. Width is the geom's rule (dtx-exact 0.35 pt
#' default) with a hard floor of `min_lwd` (default 0.5 pt, ~1 px at 96 dpi:
#' grid rasterizes sub-0.5 pt as faint grey and the 1 em arms vanish, while
#' the gold's rules always rasterize to a full-ink hairline). `min_lwd = 0`
#' restores the physically exact dtx width for print devices. The theme text
#' cannot override the floor; a theme linewidth is honoured, then floored.
#' @keywords internal
#' @noRd
rule_gp <- function(gp, rule = NULL, min_lwd = 0.5) {
  g <- grid::gpar(lwd = max((rule %||% (0.4 / 72.27)) * 72.27, min_lwd),
                  lineend = "butt", linejoin = "mitre")
  if (!is.null(gp)) {
    set <- function(nm) if (!is.null(gp[[nm]])) gp[[nm]] else g[[nm]]
    g$col <- set("col") %||% set("colour") %||% g$col
    if (!is.null(gp$lwd)) g$lwd <- gp$lwd
    if (!is.null(gp$linewidth)) g$linewidth <- gp$linewidth
  }
  g$lwd <- max(as.numeric(g$lwd), min_lwd)
  g
}
