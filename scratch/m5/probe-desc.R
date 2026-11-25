d <- "/tmp/m1"
dir.create(d, showWarnings = FALSE, recursive = TRUE)
writeLines(c(
  "Package: m1", "Title: T", "Version: 0.0.1",
  'Authors@R: person("A", "B", email = "a@b.c", role = c("aut", "cre"))',
  "Description: Does a thing.", "License: MIT + file LICENSE",
  "Encoding: UTF-8"),
  file.path(d, "DESCRIPTION"))
dir.create(file.path(d, "R"), showWarnings = FALSE)
writeLines("f <- function() 1", file.path(d, "R", "f.R"))
setwd("/tmp")
system("R CMD build m1 >/dev/null 2>&1; R CMD check --no-manual m1_0.0.1.tar.gz 2>&1 | grep -E 'checking for file|Required fields|Status:'")
