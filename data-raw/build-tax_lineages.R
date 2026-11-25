# Provenance of data/tax_lineages.rda
#
# Source: pelagic-microplastics-amplicon_from_hydra project (host of record: hydra),
# files DA_PELAGIC/results/tables/taxonomy_16S.csv and taxonomy_18S.csv — the
# SILVA-assigned ASV taxonomy tables exported from the project's phyloseq objects
# (dump script: DA_PELAGIC/R/12_dump_taxonomy.R).
#
# Selection: hand-curated 19 ASVs with complete Phylum..Genus assignments, one per
# distinct genus, chosen for clean branching (7 phyla, 6 classes, 14 orders,
# 18 distinct families over 19 rows - one row's order and family are the same
# SILVA string, Vampirovibrionales). 18S rows carry Species where the source had a real species call;
# uncultured_* placeholders and all 16S species fields are "" (no species rank in the
# 16S source). Unranked placeholders (uncultured_*, rank-shifted family==genus rows)
# were excluded.
#
# The reviewed selection snapshot is tracked in git as tax_lineages_min.csv (moved
# from data/ to data-raw/ when the package data dir was claimed by the .rda).

csv <- file.path("data-raw", "tax_lineages_min.csv")
tax_lineages <- read.csv(csv, stringsAsFactors = FALSE)
stopifnot(
  nrow(tax_lineages) == 19L,
  all(c("marker", "asv", "Phylum", "Class", "Order", "Family", "Genus", "Species")
      == names(tax_lineages)),
  !any(is.na(tax_lineages$Genus)),
  all(tax_lineages$marker %in% c("16S", "18S"))
)
save(tax_lineages, file = file.path("data", "tax_lineages.rda"), version = 2)
cat("wrote data/tax_lineages.rda:", nrow(tax_lineages), "rows\n")
