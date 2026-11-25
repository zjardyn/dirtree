test_that("display labels carry rank prefixes; × N prints on leaves only", {
  d <- dirtree_data(toy_lineages())
  lab <- dirtree:::dirtree_labels(d)
  # internal node F1 has n = 2 but must NOT print a multiplier
  i_f1 <- which(d$label == "F1" & d$level == 3L)
  expect_false(grepl("\u00d7", lab[i_f1]))
  # leaves print n only when they collapse >1 identical path
  expect_false(any(grepl("\u00d7", lab[d$level == 4L])))
  tl <- toy_lineages()
  tl <- rbind(tl, tl[1, ])  # duplicate genus path -> leaf collapse
  d2 <- dirtree_data(tl)
  lab2 <- dirtree:::dirtree_labels(d2)
  g1 <- which(d2$label == "G1" & d2$level == 4L)
  expect_identical(glengths(lab2[g1]), 1L)
  expect_true(grepl("g_G1 \u00d7 2$", lab2[g1]))
})

test_that("prefixes are by level", {
  d <- dirtree_data(toy_lineages())
  lab <- dirtree:::dirtree_labels(d)
  expect_true(all(startsWith(lab[d$level == 1L], "c_")))
  expect_true(all(startsWith(lab[d$level == 2L], "o_")))
})
