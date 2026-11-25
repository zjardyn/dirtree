test_that("display labels carry rank prefixes; × N prints on every rollup (n > 1)", {
  # Gold generator (figures/chapter3/firing_lineages_DA_sorted.tex) marks
  # multi-label paths at any rank: "f_Pirellulaceae × 2" above its child.
  d <- dirtree_data(toy_lineages())
  lab <- dirtree:::dirtree_labels(d)
  i_f1 <- which(d$label == "F1" & d$level == 3L)
  expect_identical(d$n[i_f1], 2L)          # F1 collapses two genera
  expect_true(grepl("f_F1 \u00d7 2$", lab[i_f1]))
  expect_false(any(grepl("\u00d7", lab[d$n == 1L])))
  tl <- toy_lineages()
  tl <- rbind(tl, tl[1, ])  # duplicate genus path -> leaf collapse
  d2 <- dirtree_data(tl)
  lab2 <- dirtree:::dirtree_labels(d2)
  g1 <- which(d2$label == "G1" & d2$level == 4L)
  expect_identical(length(lab2[g1]), 1L)
  expect_true(grepl("g_G1 \u00d7 2$", lab2[g1]))
})

test_that("prefixes are by level", {
  d <- dirtree_data(toy_lineages())
  lab <- dirtree:::dirtree_labels(d)
  expect_true(all(startsWith(lab[d$level == 1L], "c_")))
  expect_true(all(startsWith(lab[d$level == 2L], "o_")))
})
