test_that("display labels carry rank prefixes (rollup counts disabled for now)", {
  # The gold generator marks multi-label paths with a x N multiplier at any
  # rank; the user has shelved that mark for the time being, so labels carry
  # only the rank prefix (the n column still carries the rollup counts).
  d <- dirtree_data(toy_lineages())
  lab <- dirtree:::dirtree_labels(d)
  i_f1 <- which(d$label == "F1" & d$level == 3L)
  expect_identical(d$n[i_f1], 2L)          # F1 collapses two genera
  expect_identical(lab[i_f1], "f_F1")
  expect_false(any(grepl("\u00d7", lab)))
  tl <- toy_lineages()
  tl <- rbind(tl, tl[1, ])  # duplicate genus path -> leaf collapse
  d2 <- dirtree_data(tl)
  lab2 <- dirtree:::dirtree_labels(d2)
  g1 <- which(d2$label == "G1" & d2$level == 4L)
  expect_identical(length(lab2[g1]), 1L)
  expect_identical(lab2[g1], "g_G1")
})

test_that("prefixes are by level", {
  d <- dirtree_data(toy_lineages())
  lab <- dirtree:::dirtree_labels(d)
  expect_true(all(startsWith(lab[d$level == 1L], "c_")))
  expect_true(all(startsWith(lab[d$level == 2L], "o_")))
})
