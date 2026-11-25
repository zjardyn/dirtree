# Tree geometry: node table + break positions -> drawing coordinates.
#
# Pure data: no grid, no ggplot2. The connector model re-derives the drawing
# rules of the LaTeX dirtree package v0.32 (template/dirtree.dtx):
#   - \DTsetlength ratios: label gap 0.2em, rule width 0.4pt; successive
#     ranks indent by the width of the parent's label column (gold standard:
#     every rank's label starts one tab stop right of its parent's). The
#     minimum column width is one pitch (1.4 em), so empty columns stay
#     visible. (Junction dots, \DT@dotwidth = 1.6pt, are dropped in v1 —
#     plan Decision Log.)
#   - Node connector ("|_"): a rule in the parent's trunk column runs from
#     the parent's row across the node's sibling block; a short connector
#     joins that column to each child's label. Tee at non-last children
#     (block continues), elbow at the last child.
#   - Roots hang off a free root trunk at x = pitch/2: one continuous rule
#     from the axis line across all roots, tee at every root except the last
#     (gold-standard pixel analysis: the leftmost rule is a single span
#     y=39→1150 at ~0.7 em; plan round-2 BLOCKER-1).
#
# Coordinates are NORMALISED: "along" = position along the axis band (the
# row slot, mapped to npc break positions by the caller), "depth" = rank
# indentation in inches (column 0 is the axis line; the band's top for the
# x-axis is column 0). `widths` gives the label-column width per rank level
# (inches, from the renderer via grid's text metrics; the pure-data core
# never measures text).

dirtree_geom <- function(pitch_em = 1.4, gap_em = 1.2, rule_pt = 0.4,
                         row_em = 1.2, em_in = 12 / 72.27) {
  list(
    pitch = pitch_em * em_in,
    gap = gap_em * em_in,
    rule = rule_pt / 72.27,
    row = row_em * em_in,
    em = em_in
  )
}

# nodes: node table (label, level, parent, n) in tree order with parents
# preceding; pos: length-nrow row-slot positions (npc, 0..1, caller's
# direction); widths: label-column width per level (inches); text_len:
# rendered label length per node (inches, for the band extent of rotated
# x-axis labels).
dirtree_layout <- function(nodes, pos, widths, text_len = NULL,
                          geom = dirtree_geom()) {
  n <- nrow(nodes)
  if (n == 0L) {
    return(NULL)
  }
  lvl <- nodes$level
  max_lvl <- max(lvl)
  widths <- pmax(as.numeric(widths), geom$pitch)  # floor: one pitch
  # start[level] = depth of the parent's trunk column for children of that
  # level: level-L labels start at start[L]; the trunk column elbowing into
  # them sits 0.2em to their left. Root trunk at pitch/2 (gold).
  start <- numeric(max_lvl + 1L)
  start[1L] <- geom$pitch / 2
  if (max_lvl >= 1L) {
    start[seq_len(max_lvl) + 1L] <- geom$pitch / 2 + cumsum(widths)[seq_len(max_lvl)]
  }
  x_conn <- start[lvl + 1L] + 0.2 * geom$em      # elbow corner of each node
  x_label <- x_conn + 0.2 * geom$em              # label anchor (rank descent)
  # Each node's elbow starts at its trunk column: roots on the free trunk,
  # children on their parent's trunk column (the parent's elbow corner minus
  # the 0.2em gutter, where the parent's rule actually runs).
  x_trunk <- ifelse(lvl == 0L, geom$pitch / 2,
                    x_conn[as.integer(nodes$parent)] - 0.2 * geom$em)
  y_label <- (lvl + 1L) * geom$row               # stub depth below axis line
  depth <- max(x_label) + geom$gap               # rank spread (perpendicular)
  band <- if (is.null(text_len)) {
    max(y_label) + 0.4 * geom$row
  } else {
    max(x_label + as.numeric(text_len)) + 0.2 * geom$em
  }

  # Connector parent: the node table's parent (the guide remaps it to the
  # sliced break subset; a valid depth-first table's parent is the nearest
  # preceding row at level - 1, i.e. dirtree's \DT@countiv, and the
  # construction works mirrored, e.g. a top-down y-axis).
  cp <- as.integer(nodes$parent)

  kid <- !is.na(cp)
  last_child <- !duplicated(cp, fromLast = TRUE)
  last_child[!kid] <- TRUE

  # Trunk block per connector-parent: from the parent's row to its farthest
  # child's row (covers the whole sibling block in tree order, forward or
  # reversed).
  blocks <- .rle_rows(cp[kid], cp_row = which(kid))
  cp2 <- blocks$cp
  farthest <- ifelse(abs(blocks$end - cp2) >= abs(blocks$start - cp2),
                     blocks$end, blocks$start)
  x_blk <- x_conn[blocks$cp]
  y0_blk <- blocks$cp
  y1_blk <- farthest
  # Roots: the roots form one (contiguous, in tree order) NA-parent block;
  # they share the free trunk at x = pitch/2, spanning first root's row to
  # the last root's row, tee at every root except the last.
  root_blk <- which(blocks$root)
  if (length(root_blk)) {
    keep <- !blocks$root
    x_blk <- c(x_blk[keep], geom$pitch / 2)
    y0_blk <- c(y0_blk[keep], max(blocks$end[root_blk]))
    y1_blk <- c(y1_blk[keep], min(blocks$start[root_blk]))
  }
  trunk <- list(
    x = x_blk,
    y0 = pos[y0_blk],
    y1 = pos[y1_blk]
  )
  tick <- list(                                # short stub from elbow to label
    x0 = x_conn[kid], x1 = x_conn[kid],
    y0 = pos[kid], y1 = pos[kid]
  )
  list(
    x = pos, y = y_label, x_label = x_label, x_conn = x_conn, x_trunk = x_trunk,
    trunk = trunk, tick = tick,
    depth = depth,                                   # rank spread (inches)
    band = band,                                     # deepest label extent
    geom = geom
  )
}

# Run-length blocks of children grouped by connector-parent, in row order.
# Returns start/end row indices (into the full table) and root flag.
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

# Per-node display labels: rank prefix; " × N" on terminal nodes of the
# emitted table only (rows with no children; the gold standard's genus
# multiplier). Prefix set mirrors the gold standard p_/c_/o_/f_/g_ (s_ for
# species).
dirtree_labels <- function(nodes, prefixes = c("p_", "c_", "o_", "f_", "g_", "s_")) {
  pre <- ifelse(nodes$level < length(prefixes),
                prefixes[nodes$level + 1L],
                paste0("#", nodes$level, "_"))
  leaf <- !(seq_len(nrow(nodes)) %in% nodes$parent[!is.na(nodes$parent)])
  mult <- ifelse(leaf & nodes$n > 1L, paste0(" \u00d7 ", nodes$n), "")
  paste0(pre, nodes$label, mult)
}
