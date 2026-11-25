# Shared fixtures for dirtree tests. All self-contained: no package-data
# dependency except `tax_lineages` (shipped), no external files.

# Two genera share family F1 (rollup predicate); a third lineage branches
# at the root. Levels 0..4, 11 nodes (2 roots: P1 shared chain + P2).
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
