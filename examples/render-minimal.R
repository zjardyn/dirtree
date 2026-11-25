# Renders the minimal example (run in the dev container: .dev/r Rscript ...).
# Builds a throwaway installed copy of the package tree (R/ + data/) so the
# real namespace is used, then plots. Roxygen-generated NAMESPACE is stubbed.
build_shim <- function(root) {
  shim <- file.path(tempdir(), "dirtree")
  dir.create(shim, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(root, pattern = "^R$|data|^DESCRIPTION", full.names = TRUE),
            shim, recursive = TRUE)
  writeLines(readLines(file.path(root, "NAMESPACE")),
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
  guides(x = guide_dirtree(nodes = d)) +
  theme_minimal() + theme(axis.text.x = element_blank())
ggsave("examples/minimal_16S.png", p, width = 11, height = 12, dpi = 300)

# y-axis (left) orientation, root at top (reversed ladder)
p2 <- ggplot(d, aes(y = node_key, x = n)) +
  geom_col() +
  scale_y_discrete(limits = rev(as.character(d$node_key)),
                   breaks = rev(d$node_key)) +
  guides(y = guide_dirtree(nodes = d)) +
  theme_minimal() + theme(axis.text.y = element_blank())
ggsave("examples/minimal_16S_y.png", p2, width = 12, height = 11, dpi = 300)

# Full dataset (16S + 18S with species tier): the gold's density
d2 <- dirtree_data(tax_lineages)
p3 <- ggplot(d2, aes(y = node_key, x = n)) +
  geom_col(fill = "grey35") +
  scale_y_discrete(limits = rev(as.character(d2$node_key)),
                   breaks = rev(d2$node_key)) +
  guides(y = guide_dirtree(nodes = d2)) +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
ggsave("examples/full_lineages_y.png", p3, width = 12, height = 13, dpi = 300)
cat("wrote examples/minimal_16S.png, minimal_16S_y.png, full_lineages_y.png\n")
