# Tree geometry: node table + break positions -> drawing coordinates.
#
# Pure data: no grid, no ggplot2. The connector model reproduces the golden
# standard (pixel-verified: gold-view.R / gold-bl.R dumps), which follows the
# LaTeX dirtree package v0.32 drawing loop (template/dirtree.dtx:646-773):
#   - DT@all = offset + width + sep = 0.2em + 1em + 0.2em = 1.4 em is the
#     rank pitch (gold: trunk columns 76 px apart, em = 54.3 px). EVERY
#     row's corner column, roots included, is
#     x = level * DT@all + offset                        (dtx:666).
#   - The connector arm is a horizontal rule of width = 1 em from the node's
#     own corner column along the TEXT BASELINE (0.21 row under the slot
#     centre of centred text), then kern sep, then the label — labels start
#     1.2 em right of the corner column (dtx:757-768). The R axis model
#     mirrors this: ranks descend toward the axis line, so arms and labels
#     run toward the panel (the gold axis renders exactly that way). The
#     first row overall (first root) is unconnected.
#   - A block's trunk hangs from the PARENT'S TEXT BOX BOTTOM (0.44 row
#     under the parent's centre) down the CHILDREN's corner column to the
#     last child's arm — no stem past it (dtx:736). The roots share their
#     own corner column (0.2 em) as trunk, from the first root's centre to
#     the last root's arm (the last root is a child of that trunk).
#   - Row pitch = baselineskip; strut height 0.7 baselineskip; no junction
#     dots (gold dot-size 0).
#
# Coordinates are NORMALISED: "along" = position along the axis band (the
# row slot, mapped to npc break positions by the caller), "depth" = rank
# indentation in inches (column 0 is the axis line on the y-axis; the
# x-axis maps depth mirrored, the axis line at the band's top edge).

#' Geometry constants transcribed from LaTeX dirtree v0.32 (`\DTsetlength`):
#' offset/width/sep in em, rank pitch `DT@all = 1.4 em`, rule width, row
#' baselineskip. Defaults are the gold generator's values.
#' @keywords internal
#' @noRd
dirtree_geom <- function(offset_em = 0.2, width_em = 1, sep_em = 0.2,
                         rule_pt = 0.35, dot_pt = 1.6, row_bl = 1,
                         em_in = 12 / 72.27) {
  list(
    offset = offset_em * em_in,
    width = width_em * em_in,
    sep = sep_em * em_in,
    pitch = (offset_em + width_em + sep_em) * em_in,   # DT@all = 1.4 em
    rule = rule_pt / 72.27,
    dot = dot_pt / 72.27,
    row = row_bl * 1.2 * em_in,                        # baselineskip
    em = em_in
  )
}

#' Tree geometry: node table + break positions -> drawing coordinates.
#'
#' nodes: node table (label, level, parent, n) in tree order with parents
#' preceding; pos: length-nrow row-slot positions (npc, 0..1, caller's
#' direction); widths: ignored (rank columns are the fixed LaTeX pitch);
#' text_len: rendered label length per node (inches, for the band extent of
#' rotated x-axis labels). Pure data: no grid, no ggplot2.
#' @keywords internal
#' @noRd
#' @importFrom stats median
dirtree_layout <- function(nodes, pos, widths = NULL, text_len = NULL,
                          geom = dirtree_geom(), vertical = TRUE) {
  n <- nrow(nodes)
  if (n == 0L) {
    return(NULL)
  }
  lvl <- nodes$level
  max_lvl <- max(lvl)
  # Gold orientation (pixel-verified): the root trunk is the OUTERMOST rule
  # and ranks descend TOWARD the axis line — depth from the axis line is
  # x_conn(L) = (max_lvl - L) * pitch + offset. The 1 em arm runs from the
  # corner TOWARD the axis line (d - 1 em), then the sep gap, then the label
  # growing toward the axis line (dtx:757-768; gold labels sit between the
  # tree and the panel, the longest reaching the axis line). The whole tree
  # is shifted outward by `shift` so the longest label ends flush with the
  # axis line — the gold's proportion (tree, then labels, then bars).
  # (The x-axis guide maps these same depth values MIRRORED: the root trunk
  # lands flush at the plot axis and the labels dangle off the plot — see
  # R/grob.R depth_y.)
  x_conn <- (max_lvl - lvl) * geom$pitch + geom$offset
  label_in <- if (is.null(text_len)) 0 else as.numeric(text_len)
  shift <- if (is.null(text_len)) {
    0
  } else {
    max(0, -min(x_conn - geom$width - geom$sep - label_in))
  }
  x_conn <- x_conn + shift
  x_rule_end <- x_conn - geom$width
  x_label <- x_conn - geom$width - geom$sep
  depth <- max(x_conn) + geom$sep                    # rank spread (perpendicular)
  band <- depth

  # Connector parent: the node table's parent (the guide remaps it to the
  # sliced break subset; a valid depth-first table's parent is the nearest
  # preceding row at level - 1, i.e. dirtree's \DT@countiv, and the
  # construction works mirrored, e.g. a top-down y-axis).
  cp <- as.integer(nodes$parent)
  p1 <- cp - 1L                                      # row before a block start

  # Gold row-slot model (pixel-verified, gold-bl.R / gold-view.R dumps): the
  # text baseline sits 0.21 row UNDER the slot centre (centred text, ink
  # centre vs baseline), the connector arm lies ON that baseline; a block's
  # trunk drops from the parent's box bottom (0.44 row under the parent's
  # centre) to the LAST child's arm — no stem past it; the root trunk spans
  # first root's centre to the last root's arm on the roots' corner column.
  # Rows are axis npc; "under" is decreasing npc along the y-axis. Row units
  # are ROWS (the gold's baselineskip), converted with the row pitch — in
  # em units the arms/trunks sit a fraction of the label height off the
  # baseline and the trunk bottoms fall short of the next row. The mirrored
  # x-axis re-sets its labels top-down, so its descender side is LEFT of the
  # slot centre — the same literal minus in npc.
  pitch_npc <- if (n > 1L) stats::median(abs(diff(sort(pos)))) else 0
  # Along-axis offsets sit on the text's DESCENDER side as a TYPOGRAPHIC
  # constant (fractions of the em, via geom$row = 1.2 em), NOT of the axis
  # row pitch: at sparse pitch a row-fraction offset opens a canyon between
  # a parent's elbow and its child's trunk (the "disconnected" defect).
  # The offsets are returned in INCHES and applied at draw time, where the
  # viewport height converts them to npc.
  # arm (baseline) = 0.21 row = 0.252 em; parent box bottom = 0.44 row =
  # 0.528 em; root span start = 0.02 row = 0.024 em — the gold's values.
  arm_y <- pos
  leg_top <- pos
  root_top_y <- pos
  yoff <- list(arm = 0.21 * geom$row, box = 0.44 * geom$row,
               root = 0.02 * geom$row)

  kid <- !is.na(cp)
  # Block membership: a child is the FIRST child of its parent if the
  # preceding row is not its sibling (parent heads the block, dtx:744).
  first_child <- kid & (seq_len(n) == 1L | is.na(p1) |
                          (p1 >= 1L & p1 <= n & cp[seq_len(n)][pmax(p1, 1L)] != cp))

  # Trunk segments, one per child: children of the same parent tile the span
  # from the parent's box bottom to the last child's arm (each segment runs
  # from the previous child's arm down to this child's arm on the child's
  # corner column). A child whose rank level jumps past elided ranks
  # (hits-only trees; recorded in the `skip` column) draws its stretch
  # DOTTED so the skipped ranks read on the connector; the rest stays solid.
  kid_rows <- seq_len(n)[kid]
  x_blk <- x_conn[kid_rows]                 # the children's column
  y0_blk <- numeric(length(kid_rows))
  y1_blk <- arm_y[kid_rows]
  y0k <- character(length(kid_rows))        # offset kind of each trunk top
  y1k <- rep("arm", length(kid_rows))       # trunk bottoms all end at arms
  sk_col <- if (is.null(nodes$skip)) rep(0, n) else as.numeric(nodes$skip)
  trunk_dotted <- logical(length(kid_rows))
  last_val <- leg_top                       # numeric: parent's current top y
  lastk <- rep("box", n)                    # kind of that top (box until a child lands)
  for (i in seq_along(kid_rows)) {
    r <- kid_rows[i]
    p <- cp[r]
    y0_blk[i] <- last_val[p]
    y0k[i] <- lastk[p]
    trunk_dotted[i] <- if (!is.null(nodes$skip)) sk_col[r] > 0L else (lvl[r] - lvl[p] > 1L)
    last_val[p] <- arm_y[r]
    lastk[p] <- "arm"
  }
  # Connector arm visibility: the first row's arm is suppressed in the gold
  # (nothing precedes it), but a hits-only head with elided ranks above it
  # must show its connector.
  show <- seq_len(n) > 1L
  if (!is.null(nodes$skip) && nodes$skip[1] > 0) show[1] <- TRUE
  # Root trunk / forest spine:
  #  * contiguous same-level roots (the gold layout) -> one span on their
  #    shared corner column, first root's centre to last root's arm;
  #  * scattered or mixed-level roots (hits-only / trie trees) -> a SPINE
  #    one column outside the deepest rank, with an elbow from the spine
  #    into every root's own corner column. The spine sits outside all
  #    subtrees, so it ties the forest together without slashing through
  #    any subtree's rows.
  if (any(!kid)) {
    rt <- which(!kid)
    contiguous <- all(diff(rt) == 1L)
    same_lvl <- length(unique(lvl[rt])) == 1L
    if (same_lvl && contiguous) {
      x_blk <- c(x_blk, x_conn[rt[1L]])
      y0_blk <- c(y0_blk, root_top_y[rt[1L]])
      y0k <- c(y0k, "root")
      y1_blk <- c(y1_blk, arm_y[rt[length(rt)]])
      y1k <- c(y1k, "arm")
      trunk_dotted <- c(trunk_dotted, FALSE)
    } else {
      # one full pitch outside the (shifted) outermost root column, so the
      # spine always sits clear of every rank column and label
      spine_d <- max(x_conn) + geom$pitch
      x_blk <- c(x_blk, spine_d)
      y0_blk <- c(y0_blk, root_top_y[rt[1L]])
      y0k <- c(y0k, "root")
      y1_blk <- c(y1_blk, arm_y[rt[length(rt)]])
      y1k <- c(y1k, "arm")
      trunk_dotted <- c(trunk_dotted, FALSE)
      # the spine lives outside the rank columns: widen the band so the
      # axis reservation (and the grob's clipping frame) includes it
      depth <- spine_d + geom$sep
      band <- depth
      spine_elbow_x0 <- c(x_conn, rep(spine_d, length(rt)))
      spine_elbow_x1 <- c(x_rule_end, x_conn[rt])
      spine_elbow_y <- c(arm_y, arm_y[rt])
      spine_elbow_yk <- c(rep("arm", n), rep("arm", length(rt)))
      spine_show <- c(show, rep(TRUE, length(rt)))
    }
  }
  trunk <- list(x = x_blk, y0 = y0_blk, y1 = y1_blk, y0k = y0k, y1k = y1k,
                dotted = trunk_dotted)
  tick <- list(
    x0 = if (exists("spine_elbow_x0")) spine_elbow_x0 else x_conn,
    x1 = if (exists("spine_elbow_x1")) spine_elbow_x1 else x_rule_end,
    y0 = if (exists("spine_elbow_y")) spine_elbow_y else arm_y,
    y1 = if (exists("spine_elbow_y")) spine_elbow_y else arm_y,
    yk = if (exists("spine_elbow_yk")) spine_elbow_yk else rep("arm", n),
    show = if (exists("spine_show")) spine_show else show
  )
  list(
    x = pos, y = pos, x_label = x_label, x_conn = x_conn, x_trunk = x_conn,
    trunk = trunk, tick = tick, yoff = yoff,
    arm = arm_y, first_child = first_child,
    depth = depth,                                   # rank spread (inches)
    band = band,                                     # deepest trunk column + sep
    geom = geom
  )
}

#' Run-length blocks of consecutive rows sharing a connector-parent, in row
#' order. Returns start/end row indices; the caller passes only child rows
#' (roots have no block — the forest rule in dirtree_layout draws the single
#' root trunk instead).
#' @keywords internal
#' @noRd
.rle_rows <- function(cp_child, cp_row) {
  r <- rle(cp_child)
  end <- cumsum(r$lengths)
  start <- end - r$lengths + 1L
  list(
    cp = r$values,
    start = cp_row[start],
    end = cp_row[end]
  )
}

#' Per-node display labels: rank prefix; " × N" on EVERY node whose label is
#' a rollup (n > 1) — the gold generator
#' (figures/chapter3/firing_lineages_DA_sorted.tex) shows
#' "f_Pirellulaceae × 2" above its child row, "o_Planctomycetales × 3",
#' "p_SAR324 clade × 4": multi-label paths are marked at any rank. Prefix set
#' mirrors the gold p_/c_/o_/f_/g_ (s_ for species), typewriter style.
#' @keywords internal
#' @noRd
dirtree_labels <- function(nodes, prefixes = c("p_", "c_", "o_", "f_", "g_", "s_")) {
  paste0(.dirtree_prefix_vec(nodes$level, prefixes), nodes$label)
}
