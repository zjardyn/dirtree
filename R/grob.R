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
# arm. On the x-axis the tree is MIRRORED into the axis band (user rule: the
# trunk sits flush with the plot axis and the leaves dangle off the plot):
# the root trunk lands just under the panel edge, ranks descend AWAY from
# the axis, and labels read top-down (rot -90) growing off the plot. The
# geometry formula is the gold's unchanged; only the depth->device mapping
# flips (depth d = d inches up from the band's outer edge).

dirtree_grob <- function(nodes, labels, pos, gp = NULL, geom = NULL,
                        vertical = FALSE) {
  .check_dirtree_inputs(nodes, labels, pos)
  gp <- gp %||% grid::gpar()
  fontsize <- gp$fontsize %||% gp$size %||% 12
  # Geometry/text decoupling: DIRTREE_EM_PT fixes the em (arm/gap/column
  # pitch) independently of the label fontsize, DIRTREE_RULE_PT the rule
  # weight — lets dense figures keep gold-proportioned bars under small text.
  em_pt <- suppressWarnings(as.numeric(Sys.getenv("DIRTREE_EM_PT", "")))
  rule_pt <- suppressWarnings(as.numeric(Sys.getenv("DIRTREE_RULE_PT", "")))
  em_in <- if (is.finite(em_pt) && em_pt > 0) em_pt / 72.27 else fontsize / 72.27
  rp <- if (is.finite(rule_pt) && rule_pt > 0) rule_pt else 0.35
  geom <- geom %||% dirtree_geom(em_in = em_in, rule_pt = rp)
  # Gold typography: typewriter labels, genus/species NAMES italic
  # (\texttt{g\_}{\itshape ...}). Courier advance width is exactly 0.6 em,
  # so text_len is exact, not a heuristic.
  face <- ifelse(grepl("^[gs]_", labels), "italic", "plain")
  gp <- grid::gpar(
    fontfamily = "mono", fontface = face, fontsize = fontsize,
    col = gp$col %||% grid::gpar("col")$col
  )
  # Label width: the gold assumed LaTeX Courier's exact 0.6 em advance, but
  # cairo devices map "mono" to a wider monospace (~1.1 em measured), which
  # under-reserved the axis band and pushed chains off the figure. Measure
  # the device's actual advance for the label font when one is open; fall
  # back to the gold's 0.6 em otherwise (pure pdf() keeps Courier metrics).
  adv_em <- tryCatch({
    grid::pushViewport(grid::viewport(gp = grid::gpar(
      fontfamily = gp$fontfamily %||% "mono", fontsize = fontsize)))
    on.exit(grid::popViewport(), add = TRUE)
    grid::convertWidth(
      grid::stringWidth(strrep("0", 64L)), "in", valueOnly = TRUE
    ) / 64L / (fontsize / 72.27)
  }, error = function(e) 0.67)
  # Reserve with a 15% safety margin over the build-time measurement: the
  # split labels are seam-pinned at draw time, so the reservation only has
  # to be generous, never exact.
  text_len <- nchar(as.character(labels)) * adv_em * fontsize / 72.27 * 1.15
  ly <- dirtree_layout(nodes, pos, text_len = text_len, geom = geom,
                       vertical = vertical)
  if (is.null(ly)) {
    return(grid::grid.null())
  }
  along <- function(a) grid::unit(as.numeric(a), "npc")
  # Depth coordinates run from the axis line into the band. For the x-axis
  # the axis line is the band TOP: the tree hangs from it, so depth d maps
  # to d inches UP from the band's outer (bottom) edge — the root trunk
  # (largest depth) lands just under the panel edge. (For the y-axis — a
  # left axis — the axis line is the band's RIGHT edge, so depth runs from
  # 1npc leftward.) The band's measured size equals the tree's deepest
  # extent, so both mappings resolve against the band viewport.
  depth_x <- function(d) {
    if (vertical) grid::unit(1, "npc") - grid::unit(d, "inches")
    else grid::unit(d, "inches")
  }
  depth_y <- function(d) grid::unit(d, "inches")
  # Axis-aware placement: a point (row r, depth d). Rows keep the break
  # order (r = break npc); the x-axis grows the tree downward from the band
  # top, the y-axis leftward from the axis line.
  px <- function(r, d) if (vertical) depth_x(d) else along(r)
  py <- function(r, d) if (vertical) along(r)   else depth_y(d)
  # Dissolve collinear trunk segments: nested blocks re-paint the same
  # corner column, and stacked identical rules render darker/thicker under
  # PDF antialiasing. Merge per-column intervals to their union so each
  # column's covered span is painted exactly once (dtx draws the same
  # geometry, but its single loop paints each column once per block with
  # butt joins at identical colour — the union is visually identical and
  # rasterisation-proof). Dotted (elided-rank) and solid spans merge
  # separately so the skip marking survives.
  tr <- .dissolve_trunks(ly$trunk$x, ly$trunk$y0, ly$trunk$y1, ly$trunk$dotted)
  gp_tr <- rule_gp(gp, ly$geom$rule)
  gp_tr$lty <- if (length(tr$dotted)) ifelse(tr$dotted, "dotted", "solid") else "solid"
  trunk <- grid::segmentsGrob(
    x0 = px(tr$y0, tr$x),
    y0 = py(tr$y0, tr$x),
    x1 = px(tr$y1, tr$x),
    y1 = py(tr$y1, tr$x),
    gp = gp_tr, name = "dirtree-rules"
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
    gp = rule_gp(gp, ly$geom$rule), name = "dirtree-arms"
  )
  # Labels may carry a dropped-rank chain before the hit itself
  # ("o_X;f_Y;g_Z" in hits-only trees): the chain draws grey (scaffold
  # ancestry), the hit keeps the theme colour — grey again only if the hit
  # itself is an unresolved placeholder ("I.S." / "Unknown"). The seam
  # between the two is resolved at DRAW time (device metrics), so it stays
  # exact on any font: a classed grob measures the full label and the hit
  # part under the real device, then pins the hit's start there and
  # right-aligns the grey chain against it.
  # a leading \x01 sentinel marks a whole-grey label (e.g. shared
  # untested-ancestry rows); it is stripped before drawing
  grey_node <- startsWith(labels, "\x01")
  labels <- sub("^\x01", "", labels)
  has_chain <- grepl(";", labels)
  chain_part <- ifelse(has_chain, sub(";[^;]*$", "", labels), "")
  hit_part <- ifelse(has_chain, sub("^.*;", "", labels), labels)
  gp_hit <- gp
  ph <- grepl("I\\.S\\.|Unknown", hit_part) | grey_node
  if (any(ph)) gp_hit$col <- ifelse(ph, "grey45", gp$col %||% "black")
  gp_hit$fontface <- ifelse(grepl("^[gs]_", hit_part), "italic", "plain")
  gp_hit$font <- NULL
  gp_chain <- gp
  gp_chain$col <- "grey50"
  gp_chain$fontface <- "plain"
  gp_chain$font <- NULL
  if (vertical) {
    txt <- grid::gList(grid::grob(
      label = labels, hit = hit_part,
      chain = ifelse(has_chain, paste0(chain_part, ";"), ""),
      x_depth = ly$x_label, y_row = ly$x,
      gp_hit = gp_hit, gp_chain = gp_chain,
      fontsize = fontsize, cl = "dirtreeSplitLabel"
    ))
  } else {
    txt <- grid::gList(grid::textGrob(
      label = labels,
      x = along(ly$x),
      y = depth_y(ly$x_label),
      hjust = 1,
      vjust = 0.5,
      rot = 90,
      gp = gp_hit, name = "label-hit"
    ))
  }
  grid::gTree(
    children = grid::gList(trunk, hrule,
                           grid::gTree(children = txt,
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

#' Draw the split labels: the chain grey, the hit in theme colour, with the
#' seam measured under the REAL device at draw time (build-time font
#' metrics differ from draw-time rendering on cairo). The hit's start is
#' pinned at label start + full width - hit width, and the grey chain is
#' right-aligned against that same point, so the two can never overlap
#' however wide the device mono font actually is.
#' @keywords internal
#' @noRd
drawDetails.dirtreeSplitLabel <- function(x, recording = TRUE) {
  grid::pushViewport(grid::viewport(gp = grid::gpar(
    fontfamily = "mono", fontsize = x$fontsize)))
  on.exit(grid::popViewport(), add = TRUE)
  w_total <- grid::convertWidth(
    grid::stringWidth(x$label), "in", valueOnly = TRUE)
  w_hit <- grid::convertWidth(
    grid::stringWidth(x$hit), "in", valueOnly = TRUE)
  seam <- x$x_depth - (w_total - w_hit)   # depth where the hit starts
  grid::grid.text(
    label = x$hit,
    x = grid::unit(1, "npc") - grid::unit(seam, "inches"),
    y = grid::unit(x$y_row, "npc"),
    just = c(0, 0.5), gp = x$gp_hit
  )
  chn <- x$chain != ""
  if (any(chn)) {
    grid::grid.text(
      label = x$chain[chn],
      x = grid::unit(1, "npc") - grid::unit(seam[chn], "inches"),
      y = grid::unit(x$y_row[chn], "npc"),
      just = c(1, 0.5), gp = x$gp_chain
    )
  }
  invisible(TRUE)
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
#' @rawNamespace S3method(grid::drawDetails, dirtreeSplitLabel)
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

#' Merge collinear overlapping vertical trunk segments (layout coordinates:
#' depth `x` fixed per column, along-axis span `y0..y1`) into per-column
#' unions, so nested blocks sharing a corner column paint each covered span
#' exactly once instead of stacking identical rules.
#' @keywords internal
#' @noRd
.dissolve_trunks <- function(x, y0, y1, dotted = NULL) {
  stopifnot(length(x) == length(y0), length(y0) == length(y1))
  if (is.null(dotted)) dotted <- rep(FALSE, length(x))
  stopifnot(length(dotted) == length(x))
  if (!length(x)) {
    return(list(x = x, y0 = y0, y1 = y1, dotted = dotted))
  }
  lo <- pmin(y0, y1)
  hi <- pmax(y0, y1)
  gkey <- paste(x, dotted)
  ord <- order(gkey, lo)
  x <- x[ord]; lo <- lo[ord]; hi <- hi[ord]; dotted <- dotted[ord]
  gkey <- gkey[ord]
  new_x <- new_lo <- new_hi <- numeric(0)
  new_dot <- logical(0)
  i <- 1L
  n <- length(x)
  while (i <= n) {
    j <- i
    while (j < n && gkey[j + 1L] == gkey[j]) j <- j + 1L
    cur_lo <- lo[i]; cur_hi <- hi[i]
    if (j > i) {
      for (k in (i + 1L):j) {
        if (lo[k] <= cur_hi) {
          cur_hi <- max(cur_hi, hi[k])
        } else {
          new_x <- c(new_x, x[i]); new_lo <- c(new_lo, cur_lo); new_hi <- c(new_hi, cur_hi)
          new_dot <- c(new_dot, dotted[i])
          cur_lo <- lo[k]; cur_hi <- hi[k]
        }
      }
    }
    new_x <- c(new_x, x[i]); new_lo <- c(new_lo, cur_lo); new_hi <- c(new_hi, cur_hi)
    new_dot <- c(new_dot, dotted[i])
    i <- j + 1L
  }
  list(x = new_x, y0 = new_lo, y1 = new_hi, dotted = new_dot)
}

#' Rule gpar: thin butt-capped lines inheriting colour from the theme text,
#' drawn under the labels. Width is the geom's rule (dtx-exact 0.35 pt
#' default) with a hard floor of `min_lwd` (default 0.5 pt, ~1 px at 96 dpi:
#' grid rasterizes sub-0.5 pt as faint grey and the 1 em arms vanish, while
#' the gold's rules always rasterize to a full-ink hairline). `min_lwd = 0`
#' restores the physically exact dtx width for print devices. A theme `lwd`
#' is honoured, then floored. grid consumes the `lwd` field; `linewidth` is
#' never set here because it does not render for grid lines.
#' @keywords internal
#' @noRd
rule_gp <- function(gp, rule = NULL, min_lwd = 0.5) {
  g <- grid::gpar(lwd = max((rule %||% (0.4 / 72.27)) * 72.27, min_lwd),
                  lineend = "butt", linejoin = "mitre")
  if (!is.null(gp)) {
    set <- function(nm) if (!is.null(gp[[nm]])) gp[[nm]] else g[[nm]]
    g$col <- set("col") %||% set("colour") %||% g$col
    if (!is.null(gp$lwd)) g$lwd <- gp$lwd
  }
  g$lwd <- max(as.numeric(g$lwd), min_lwd)
  g
}
