# dirtree — R package build brief

Build an R package that renders ranked taxonomic hierarchies as text "ladder" trees.

## Inputs

- `data/tax_lineages_min.csv` — minimal hierarchical dataset (schema in `data/README.md`):
  `marker`, `asv` (md5 hash), `Phylum`…`Genus`, `Species` (18S only). 19 complete
  phylum→genus lineages from the pelagic microplastics amplicon project.
- `examples/golden_standard.png` — the exact visual output the renderer must reproduce
  (conventions listed in `examples/README.md`): `p_`/`c_`/`o_`/`f_`/`g_` prefixes,
  elbow connectors, rank-depth columns, gray upper ranks, bold/italic black genera,
  `× N` collapse of identical lineages.

## Template source code

`template/` (copied from `~/Library/Mobile Documents/com~apple~CloudDocs/dirtree/`)
— the LaTeX **dirtree** package v0.32 (Jean-Côme Charpentier, LPPL), docstrip source.
Use its model and geometry as the template for the R implementation:

- `dirtree.dtx` — all code + documentation. Core: `\dirtree{...}` with items
  `[.] name <level>`; per-node column width = `offset + width + sep`
  (`\DT@offset`=0.2em, `\DT@width`=1em, `\DT@sep`=0.2em, rule 0.4pt, default dot 1.6pt),
  settable via `\DTsetlength`; node styling via `\DTstyle` / `\DTstylecomment`
  (`\DTcomment` = the trailing-comment mechanism → the `× N` collapse label);
  options `rightalign`, `dots`, `breakable`; vertical rhythm `\DTbaselineskip`.
- `dirtree.ins` — docstrip driver; `dirtree.pdf` — rendered manual (read this first
  for the option semantics and example trees).

Map the LaTeX concepts to R: node level → rank depth, `\DTcomment` → collapse/multiplier
label, `\DTstyle` → per-rank styling (gray p/c/o/f, bold italic g).

*License note:* the template is LPPL 1.3+; it is reference material — the R package is a
re-implementation of its ideas, not a derivative of its code, and the `.dtx` need not ship
in the built package.
