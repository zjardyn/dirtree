.libPaths(c(".R/library", .libPaths()))
pkgs <- c("ggplot2", "rlang", "vctrs", "cli", "withr",
          "devtools", "roxygen2", "testthat", "pkgload",
          "urlchecker", "styler", "spelling")
missing <- pkgs[!vapply(pkgs, requireNamespace, TRUE, quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, lib = ".R/library", repos = "https://cloud.r-project.org")
}
print(sapply(pkgs, function(p) requireNamespace(p, quietly = TRUE)))
