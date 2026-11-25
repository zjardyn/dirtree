# Tree geometry: node table -> drawing coordinates.
#
# Pure data: no grid, no ggplot2. The connector model re-derives the drawing
# rules of the LaTeX dirtree package v0.32 (template/dirtree.dtx):
#   - \DTsetlength{offset=0.2em}{width=1em}{sep=0.2em}{rulewidth=0.4pt}: each node
#     occupies a column of total pitch DT@all = offset + width + sep; the label
#     text begins at offset + width + sep into the column, i.e. every successive
#     rank shifts the label right by one pitch (dirtree draws one line per node
#     with \kern\DT@all indentation).
#   - Node connector ("|_"): a vertical rule in the parent's trunk column runs
#     from the parent's line down to the node's line; a horizontal rule joins
#     that column to the node's label. When the parent has a later child, the
#     vertical continues past this node (tee shape); at the last child it stops
#     (elbow shape). This mirrors the \DT@countiv bookkeeping
#     ("last node of a level lesser or equal to current one"), which dirtree
#     uses to decide where a level's vertical rule terminates.

# Geometry constants, in inches at a 12 pt base font (em = 12pt = 1/6 in):
# pitch = 1.4em, label gap = 1.2em (offset + width), rule = 0.4pt.
dirtree_geom <- function(pitch_em = 1.4, gap_em = 1.2, rule_pt = 0.4,
                         row_em = 1.2) {
  list(
    pitch = pitch_em / 6,
    gap = gap_em / 6,
    rule = rule_pt / 72.27,
    row = row_em / 6
  )
}

# Input: node table from dirtree_data() (depth-first, parent < self index).
# Output: coordinates for labels, ticks (horizontal), trunks (vertical).
dirtree_layout <- function(nodes, geom = dirtree_geom()) {
  nrow_nodes <- nrow(nodes)
  if (nrow_nodes == 0L) {
    return(NULL)
  }
  row <- seq_len(nrow_nodes)                      # 1 = top
  # A node at rank depth `level` prints its label in column level+2: one
  # column for the free root trunk (x = 0.5 pitch), one per enclosing rank.
  x_label <- (nodes$level + 1) * geom$pitch + geom$gap
  x_tick <- (nodes$level + 1) * geom$pitch        # elbow corner, label's column
  y <- (nrow_nodes - row + 0.5) * geom$row        # y grows upward (grid)

  # Parent trunk column: the x of the parent's elbow corner; a root (level 0)
  # hangs off the free trunk at x = 0.5*pitch, like the gold standard's
  # leftmost rule.
  x_trunk <- ifelse(is.na(nodes$parent), geom$pitch / 2, x_tick[nodes$parent])

  # Trunk y-extent per node: parent's row down to this node's row.
  y_top <- ifelse(is.na(nodes$parent), NA_real_, y[nodes$parent])

  # Does this node's parent have a child after it? (tee vs elbow)
  last_child <- !duplicated(nodes$parent, fromLast = TRUE)
  last_child[is.na(nodes$parent)] <- TRUE

  # Root trunk: one vertical from the first root to the last root (the
  # level-0 \DT@countiv column in dirtree), drawn once, tee at every root
  # except the last.
  roots <- which(is.na(nodes$parent))
  root_trunk <- NULL
  if (length(roots)) {
    root_trunk <- list(
      x = geom$pitch / 2,
      y0 = y[max(roots)],
      y1 = y[min(roots)]
    )
  }

  child <- !is.na(nodes$parent)
  segs <- list(
    # horizontal tick: parent's corner column -> label left edge
    tick = list(
      x0 = x_trunk[child], x1 = x_label[child],
      y0 = y[child], y1 = y[child]
    ),
    # vertical trunk (filled by .trunk_segments below)
    trunk = NULL
  )
  # Tee trunks: for non-last children the vertical must run to the parent's
  # LAST child, not stop here; collapse consecutive same-parent trunks into one
  # segment per parent.
  segs$trunk <- .trunk_segments(nodes, y, x_trunk, geom)

  list(
    x_label = x_label, y = y,
    tick = segs$tick,
    trunk = segs$trunk,
    root_trunk = root_trunk,
    geom = geom,
    height = nrow_nodes * geom$row,
    width = max(x_label) + geom$gap
  )
}

# One vertical segment per parent that has children: from the parent's row to
# its last child's row (dirtree: the vertical rule spans the parent's child
# block, tees at each child, ends with an elbow at the last).
.trunk_segments <- function(nodes, y, x_trunk, geom) {
  child <- !is.na(nodes$parent)
  p <- nodes$parent[child]
  x_child <- x_trunk[child]
  block <- rle(p)
  last_row <- cumsum(block$lengths)
  first_row <- last_row - block$lengths + 1L
  y_child <- y[child]
  list(
    x = x_child[first_row],
    y0 = y_child[last_row],
    y1 = y_child[first_row]
  )
}

# Per-node display labels: rank prefix; " × N" comment on LEAF nodes only
# (the gold standard shows the multiplier beside genera, never internal ranks).
# Prefix set mirrors the gold standard p_/c_/o_/f_/g_ (s_ for species level).
dirtree_labels <- function(nodes, prefixes = c("p_", "c_", "o_", "f_", "g_", "s_")) {
  pre <- ifelse(nodes$level < length(prefixes),
                prefixes[nodes$level + 1L],
                paste0("#", nodes$level, "_"))
  leaf <- !(nodes$parent %in% seq_len(nrow(nodes)))
  mult <- ifelse(leaf & nodes$n > 1L, paste0(" \u00d7 ", nodes$n), "")
  paste0(pre, nodes$label, mult)
}
