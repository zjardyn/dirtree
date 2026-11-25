# tax_lineages_min.csv

Minimal hierarchical taxonomic dataset for the R package (phylum → genus lineages,
suitable for building branched trees).

- **Source:** `../pelagic-microplastics-amplicon_from_hydra/DA_PELAGIC/results/tables/taxonomy_16S.csv`
  and `taxonomy_18S.csv` (SILVA-assigned ASV taxonomy from the pelagic microplastics
  amplicon project, copied from hydra). Hand-curated: only ASVs with complete
  Phylum/Class/Order/Family/Genus assignments, one row per distinct genus.
- **Shape:** 19 rows × 8 columns.
- **Columns:**
  - `marker` — `16S` (prokaryotes) or `18S` (eukaryotes); useful as a tree-partition/grouping variable.
  - `asv` — 32-char md5 hash of the ASV sequence (unique row id from the source table).
  - `Phylum`, `Class`, `Order`, `Family`, `Genus` — ranked hierarchy, capitalized.
  - `Species` — present **only for 18S** rows (the 16S source table has no species rank);
    blank where the source had no species assignment or only an `uncultured_*` placeholder
    (5 of 9 18S rows carry a species).
- **Branching:** 7 phyla, 11 classes, 17 orders, 19 families, 19 genera over 19
  rows (distinct labels; 16S subset 4/6/10/10/10, 18S subset 3/5/7/9/9 +
  species on 5 of 9 rows). Every
  genus and every family label is unique, so the hierarchy branches from phylum
  down (e.g. Pseudomonadota → 2 classes → 4 orders; Amoebozoa → 2 classes →
  3 orders). Two cross-rank overlaps exist: `Vampirovibrionales` is both the
  order and the family of row 7 (SILVA rank shift kept — the lineage is
  otherwise complete), and `Ochrophyta` is a class in row 18 and an order in
  row 20. The 18S subset shares the order `Fungi` (rows 15–17).
- No missing values; `uncultured_*` genera and `family == genus` rank-shifted
  rows were excluded from curation.
