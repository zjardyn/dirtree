# Shared fixtures for dirtree tests. All self-contained: no package-data
# dependency except `tax_lineages` (shipped), no external files.

# Two genera share family F1 (collapse predicate); a third lineage branches
# at the root. Levels 0..4, 8 unique paths (1 root P2 + shared chain of P1).
toy_lineages <- function() {
  data.frame(
    Phylum = c("P1", "P1", "P2"),
    Class  = c("C1", "C1", "C2"),
    Order  = c("O1", "O1", "O2"),
    Family = c("F1", "F1", "F2"),
    Genus  = c("G1", "G2", "G3"),
    stringsAsFactors = FALSE
  )
}

# Canonical depth-first level vectors (plan validation rule 2).
levels_canonical <- c(0L, 1L, 2L, 3L, 4L, 0L, 1L, 2L, 3L, 4L)
levels_broken    <- c(0L, 2L, 1L)

expect_depth_first_ok <- function(levels) {
  nodes <- dirtree:::.dirtree_nodes_from_levels(levels)
  expect_identical(nodes$level, as.integer(levels))
  expect_true(all(nodes$parent[-1] < seq_len(nrow(nodes))[-1], na.rm = TRUE))
  expect_identical(which(is.na(nodes$parent)), which(nodes$level == 0L))
}
