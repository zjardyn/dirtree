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
# indentation in inches (column 0 is the axis line; the band's top for the
# x-axis is column 0).

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
  # baseline and the trunk bottoms fall short of the next row. The rotated
  # x-axis has no along-axis baseline rule, so it keeps centred placement.
  pitch_npc <- if (n > 1L) stats::median(abs(diff(sort(pos)))) else 0
  arm_y <- if (vertical) pos - 0.21 * pitch_npc else pos
  leg_top <- if (vertical) pos - 0.44 * pitch_npc else pos
  root_top_y <- if (vertical) pos - 0.02 * pitch_npc else pos

  kid <- !is.na(cp)
  # Block membership: a child is the FIRST child of its parent if the
  # preceding row is not its sibling (parent heads the block, dtx:744).
  first_child <- kid & (seq_len(n) == 1L | is.na(p1) |
                          (p1 >= 1L & p1 <= n & cp[seq_len(n)][pmax(p1, 1L)] != cp))

  # Trunk blocks: runs of consecutive rows sharing a connector-parent. Root
  # (NA-parent) rows are NOT blocks — `rle()` never groups NA runs, so roots
  # are excluded and the forest rule below draws the one root trunk.
  kid_rows <- seq_len(n)[kid]
  blocks <- .rle_rows(cp[kid_rows], cp_row = kid_rows)
  x_blk <- x_conn[blocks$start]                       # the children's column
  y0_blk <- leg_top[blocks$cp]                        # parent's box bottom
  y1_blk <- arm_y[blocks$end]                         # last child's arm
  # Roots share their own corner column as ONE trunk, from the first root's
  # centre to the LAST root's arm (the last root is a child of that trunk,
  # dtx:736) — spanning every root even when the subtrees interleave.
  if (any(!kid)) {
    x_blk <- c(x_blk, x_conn[1L])
    y0_blk <- c(y0_blk, root_top_y[min(which(!kid))])
    y1_blk <- c(y1_blk, arm_y[max(which(!kid))])
  }
  trunk <- list(x = x_blk, y0 = y0_blk, y1 = y1_blk)
  # Connector arm into each node's label (dtx:757-768, gold): the horizontal
  # rule runs from the node's OWN corner column (x_conn) across width (1 em)
  # on the baseline row (arm_y); roots included. The first row overall (the
  # first root) is unconnected in the gold — its arm is suppressed. On the
  # rotated x-axis the same rule maps through px()/py() unchanged.
  show <- seq_len(n) > 1L
  tick <- list(
    x0 = x_conn,
    x1 = x_rule_end,
    y0 = arm_y, y1 = arm_y,
    show = show
  )
  list(
    x = pos, y = pos, x_label = x_label, x_conn = x_conn, x_trunk = x_conn,
    trunk = trunk, tick = tick,
    arm = arm_y, first_child = first_child,
    depth = depth,                                   # rank spread (inches)
    band = band,                                     # deepest label extent
    geom = geom
  )
}

#' Run-length blocks of rows grouped by connector-parent (roots are the NA
#' run), in row order. Returns start/end row indices and root flag.
#' @keywords internal
#' @noRd
.rle_rows <- function(cp_child, cp_row) {
  r <- rle(cp_child)
  end <- cumsum(r$lengths)
  start <- end - r$lengths + 1L
  list(
    cp = r$values,
    start = cp_row[start],
    end = cp_row[end],
    root = is.na(r$values)
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
  paste0(
    .dirtree_prefix_vec(nodes$level, prefixes), nodes$label,
    ifelse(nodes$n > 1L, paste0(" \u00d7 ", nodes$n), "")
  )
}
