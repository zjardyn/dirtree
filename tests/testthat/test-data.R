test_that("dirtree_data emits every node, depth-first, with subtree counts", {
  d <- dirtree_data(toy_lineages())
  # 2 phyla + 2 classes + 2 orders + 2 families + 3 genera = 11 unique paths
  expect_identical(nrow(d), 11L)
  expect_identical(d$level[1], 0L)
  expect_setequal(names(d), c("label", "level", "parent", "n", "node_key"))
  # parents precede children; only roots are NA-parented
  expect_identical(which(is.na(d$parent)), which(d$level == 0L))
  expect_true(all(d$parent[!is.na(d$parent)] < seq_len(nrow(d))[!is.na(d$parent)]))
  # depth-first: children of a node are contiguous
  kids <- split(seq_len(nrow(d)), d$parent)
  expect_true(all(vapply(kids, function(k) identical(k, seq.int(min(k), max(k))), TRUE)))
  # subtree leaf counts: family F1 has 2 genera; root P1 has 2 leaves; P2 has 1
  expect_identical(d$n[d$label == "F1" & d$level == 3L], 2L)
  expect_identical(d$n[d$label == "P1" & d$level == 0L], 2L)
  expect_identical(d$n[d$label == "P2" & d$level == 0L], 1L)
  # node_key unique, rank-prefixed
  expect_identical(length(unique(d$node_key)), nrow(d))
  expect_true(all(startsWith(d$node_key[d$level == 0], "p_")))
  expect_true(all(startsWith(d$node_key[d$level == 4], "g_")))
})

test_that("rank-string collisions across ranks get distinct node_keys", {
  tl <- toy_lineages()
  tl$Family[3] <- "O2"  # family string equals its order string (real shape:
  d <- dirtree_data(tl) # Vampirovibrionales in tax_lineages)
  expect_identical(length(unique(d$node_key)), nrow(d))
})

test_that("depth-first contract: valid tables pass, broken ones abort naming the row", {
  # plan validation rule 2, on the SHIPPING validator (.check_dirtree_node_
  # table; the dead .dirtree_nodes_from_levels helper was deleted with these
  # tests). Guard must not fire on the canonical two-root tree.
  d <- dirtree_data(toy_lineages())
  expect_true(dirtree:::.check_dirtree_node_table(d))
  bad <- d
  bad$parent[2] <- NA_integer_  # level-1 row without a parent
  expect_error(dirtree:::.check_dirtree_node_table(bad),
               "depth-first contract", fixed = TRUE)
})

test_that("ranks validation aborts RENDER their messages (B1)", {
  x <- toy_lineages()
  # the call_frame crash turned every validation into an internal error
  expect_error(dirtree_data(x, ranks = 2),
               "non-empty character", fixed = TRUE)
  expect_error(dirtree_data(x, ranks = "Nope"),
               "not found", fixed = TRUE)
})

test_that("shipped 16S subset yields 40 nodes spanning ranks 0..4", {
  d <- dirtree_data(subset(tax_lineages, marker == "16S"))
  expect_identical(nrow(d), 40L)
  expect_identical(range(d$level), c(0L, 4L))
  n_pseudo <- sum(subset(tax_lineages, marker == "16S")$Phylum == "Pseudomonadota")
  expect_identical(d$n[d$label == "Pseudomonadota"], as.integer(n_pseudo))
})

test_that("shipped 18S subset collapses at the shared Fungi order node", {
  d <- dirtree_data(subset(tax_lineages, marker == "18S"))
  fungi <- d[d$label == "Fungi", ]
  expect_identical(nrow(fungi), 1L)
  expect_identical(fungi$level, 2L)
  expect_identical(fungi$n, 3L)
})
