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
- **Branching:** 7 phyla, 6 classes, 14 orders, 18 distinct family labels over 19 rows —
  one family string (`Vampirovibrionales`) doubles as the order and family of its row,
  a within-row rank shift in the SILVA assignment kept because the lineage is otherwise
  complete. Every genus is unique, so the hierarchy branches from phylum down
  (e.g. Pseudomonadota → 2 classes → 4 orders; Amoebozoa → 2 classes → 3 orders).
- No missing values; no `uncultured_*`/rank-shifted placeholders.
