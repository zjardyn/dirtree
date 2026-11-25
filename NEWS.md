# dirtree 0.0.0.9000 (development version)

* First release: `dirtree_data()`, `guide_dirtree()`, dataset `tax_lineages`.
* Gold-mirrored connectors (dtx-exact geometry): ranks descend TOWARD the
  axis line with the root trunk outermost; each row's corner column is
  `(max rank - level) x 1.4 em + 0.2 em`; a 1 em horizontal arm sits on the
  text baseline and points toward the panel; labels start 1.2 em from their
  own trunk column, grow toward the axis line, and the longest label ends
  flush with it (the gold's proportion - tree, labels, bars). One root
  trunk spans first root to last root's arm; block trunks hang from the
  parent's box bottom down the children's column to the last child's arm.
  Typewriter labels with italic genus/species; rules 0.35 pt (`\DTsetlength`
  values from the gold generator).
