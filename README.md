# dirtree

Directory-style taxonomy trees as ggplot axis labels. `guide_dirtree()` turns a
discrete axis into a LaTeX-`dirtree`-style hierarchy: rank columns, true
tree connectors (a vertical trunk down the children's column with a short
arm into each label, ├/└ style; one root trunk spans all roots),
`p_`/`c_`/`o_`/`f_`/`g_` prefixes, and `× N`
collapse counts on rollups (` × N`, any rank with `n > 1`).

    library(dirtree)
    library(ggplot2)

    d <- dirtree_data(subset(tax_lineages, marker == "16S"))

    ggplot(d, aes(x = node_key, y = n)) +
      geom_col() +
      scale_x_discrete(limits = d$node_key, breaks = d$node_key) +
      guides(x = guide_dirtree(nodes = d)) +
      theme_minimal() +
      theme(axis.text.x = element_blank())

`dirtree_data()` expands lineage rows (phylum → genus, shipped as
`tax_lineages`) into one row per tree node, in depth-first order, with
`level`, `parent`, subtree leaf count `n`, and a unique `node_key` for the
scale breaks. The tree aligns with the panel at the break positions, so bar
plots line up row-for-row with their labels.

Development runs in Docker (`.dev/Dockerfile`, `.dev/r` wrapper); the host R
has base packages only. See `examples/golden_standard.png` for the structural
target.
