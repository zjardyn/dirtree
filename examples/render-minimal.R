# Renders the minimal example (needs ggplot2; run in the dev container).
# Builds a throwaway installed copy of the package tree (R/ + data/) so the
# real namespace is used, then plots. Roxygen-generated NAMESPACE is stubbed.
build_shim <- function(root) {
  shim <- file.path(tempdir(), "dirtree")
  dir.create(shim, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(root, pattern = "^R$|data|^DESCRIPTION", full.names = TRUE),
            shim, recursive = TRUE)
  writeLines(c(
    "export(dirtree_data)", "export(guide_dirtree)",
    "import(rlang)", "import(grid)", "import(vctrs)",
    "importFrom(ggplot2,ggproto)", "importFrom(ggplot2,waiver)",
    "importFrom(rlang,\"%||%\")"),
    file.path(shim, "NAMESPACE"))
  lib <- file.path(tempdir(), "lib")
  dir.create(lib, showWarnings = FALSE)
  install.packages(shim, repos = NULL, type = "source", lib = lib, quiet = TRUE)
  lib
}
lib <- build_shim(".")
.libPaths(c(lib, .libPaths()))
suppressMessages(library(dirtree))
suppressMessages(library(ggplot2))

d <- dirtree_data(subset(tax_lineages, marker == "16S"))
p <- ggplot(d, aes(x = node_key, y = n)) +
  geom_col() +
  scale_x_discrete(limits = d$node_key, breaks = d$node_key) +
  guides(x = guide_dirtree(levels = d$level, n = d$n)) +
  theme_minimal() + theme(axis.text.x = element_blank())
ggsave("examples/minimal_16S.png", p, width = 11, height = 12, dpi = 150)
cat("wrote examples/minimal_16S.png\n")
