# examples/

- **`golden_standard.png`** — reference output the package's tree plot must match.
  Taxonomy ladder (text tree) built from a phylum→genus table in `p_`/`c_`/`o_`/`f_`/`g_`
  prefix style, with these conventions:
  - Rank prefixes: `p_`, `c_`, `o_`, `f_`, `g_` directly on the taxon name.
  - **Colour/weight:** ranks above genus (p/c/o/f) are gray; genera are black, **bold**,
    and italicised (binomial-style genus names).
  - **Collapsing:** identical lineages are merged and labelled `× N` next to the genus
    (e.g. `g_RS25G × 2`).
  - **Elbow connectors:** each node links with a short `└`-style elbow to its parent;
    siblings share the parent's vertical trunk.
  - Depth = rank level; the tree is drawn purely from the ranked columns, no phylogram
    branch lengths.
