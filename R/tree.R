# Tree geometry: node table + break positions -> drawing coordinates.
#
# Pure data: no grid, no ggplot2. The connector model transcribes the drawing
# rules of the LaTeX dirtree package v0.32 (template/dirtree.dtx), whose
# drawing loop (dtx:646-773) is:
#   - DT@all = offset + width + sep = 0.2em + 1em + 0.2em = 1.4 em is the
#     rank pitch (gold image: trunk columns 76 px apart, em = 54.3 px);
#     a level-L node's vertical trunk rule stands at
#     x = (L-1) * DT@all + offset                                   (dtx:666).
#   - The rule runs from the connected node's (\DT@countiv: nearest
#     preceding row with level <= L-1) baseline down +0.5 baselineskip,
#     shortened by 0.5 baselineskip when the connection starts at a parent
#     (first child of the block)                                      (dtx:736).
#   - At the node's baseline: junction dot (1.6pt square, dropped in v1),
#     a horizontal rule of width = 1em, kern sep = 0.2em, then the label
#     (dtx:757-768) -> label starts 1.2 em right of the trunk column.
#   - Row pitch = baselineskip; strut height 0.7 baselineskip.
# Forest adaptation: the package has one root with no connector (dtx:602);
# a taxonomic forest hangs its roots off a free trunk at pitch/2 spanning
# first..last root (gold pixel analysis: one continuous rule at ~0.7 em
# across all roots; plan round-2 BLOCKER-1).
#
# Coordinates are NORMALISED: "along" = position along the axis band (the
# row slot, mapped to npc break positions by the caller), "depth" = rank
# indentation in inches (column 0 is the axis line; the band's top for the
# x-axis is column 0).

dirtree_geom <- function(offset_em = 0.2, width_em = 1, sep_em = 0.2,
                         rule_pt = 0.4, dot_pt = 1.6, row_bl = 1,
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

# nodes: node table (label, level, parent, n) in tree order with parents
# preceding; pos: length-nrow row-slot positions (npc, 0..1, caller's
# direction); widths: ignored (rank columns are the fixed LaTeX pitch);
# text_len: rendered label length per node (inches, for the band extent of
# rotated x-axis labels).
dirtree_layout <- function(nodes, pos, widths = NULL, text_len = NULL,
                          geom = dirtree_geom()) {
  n <- nrow(nodes)
  if (n == 0L) {
    return(NULL)
  }
  lvl <- nodes$level
  max_lvl <- max(lvl)
  # Trunk (elbow corner) column of a level-L node (dtx:666 phase,
  # zero-based levels: the rule stands 0.2 em into the level's 1.4-em
  # column); roots on the free trunk at pitch/2 (forest adaptation, gold
  # leftmost rule).
  x_conn <- ifelse(lvl == 0L, geom$pitch / 2,
                   lvl * geom$pitch + geom$offset)
  # Label starts width + sep (1.2 em) right of the trunk column (dtx:763-764);
  # the horizontal rule spans from the column across width (1 em), the label
  # follows after the sep gap.
  x_rule_end <- x_conn + geom$width
  x_label <- x_conn + geom$width + geom$sep
  # dtx:736-747: the vertical rule reaches 0.5 baselineskip past the child's
  # baseline; it starts 0.5 baselineskip under the parent's baseline when
  # the parent heads the block (first child), and at the previous sibling's
  # baseline otherwise.
  stem <- 0.5 * geom$row
  depth <- max(x_label) + geom$sep                   # rank spread (perpendicular)
  band <- if (is.null(text_len)) {
    depth
  } else {
    max(x_conn + as.numeric(text_len)) + geom$sep
  }

  # Connector parent: the node table's parent (the guide remaps it to the
  # sliced break subset; a valid depth-first table's parent is the nearest
  # preceding row at level - 1, i.e. dirtree's \DT@countiv, and the
  # construction works mirrored, e.g. a top-down y-axis).
  cp <- as.integer(nodes$parent)
  p1 <- cp - 1L                                      # row before a block start

  kid <- !is.na(cp)
  # Block membership: a child is the FIRST child of its parent if the
  # preceding row is not its sibling (parent heads the block, dtx:744).
  first_child <- kid & (seq_len(n) == 1L | is.na(p1) |
                          (p1 >= 1L & p1 <= n & cp[seq_len(n)][pmax(p1, 1L)] != cp))
  last_child <- kid & !duplicated(cp, fromLast = TRUE)

  # Trunk blocks: runs of consecutive rows sharing a connector-parent.
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
  # Elbow per node: horizontal rule width 1 em + sep gap, at the node's row
  # from its trunk column (dtx:763-764). Roots' rule spans pitch/2 -> label.
  tick <- list(
    x0 = x_conn, x1 = x_rule_end,
    y0 = pos, y1 = pos
  )
  list(
    x = pos, y = pos, x_label = x_label, x_conn = x_conn, x_trunk = x_conn,
    trunk = trunk, tick = tick,
    stem = stem, first_child = first_child,
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

# Per-node display labels: rank prefix; " × N" on EVERY node whose label is
# a rollup (n > 1) — the gold generator (figures/chapter3/firing_lineages_DA_sorted.tex)
# shows "f_Pirellulaceae × 2" above its child row, "o_Planctomycetales × 3",
# "p_SAR324 clade × 4": multi-label paths are marked at any rank. Prefix set
# mirrors the gold p_/c_/o_/f_/g_ (s_ for species), typewriter style.
dirtree_labels <- function(nodes, prefixes = c("p_", "c_", "o_", "f_", "g_", "s_")) {
  pre <- ifelse(nodes$level < length(prefixes),
                prefixes[nodes$level + 1L],
                paste0("#", nodes$level, "_"))
  mult <- ifelse(nodes$n > 1L, paste0(" \u00d7 ", nodes$n), "")
  paste0(pre, nodes$label, mult)
}
