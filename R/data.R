#' Pelagic microplastics amplicon taxonomic lineages
#'
#' A minimal hierarchical dataset: 19 hand-curated amplicon sequence variants
#' (ASVs) from the pelagic microplastics degradation project, one per distinct
#' genus, with complete assignments from phylum to genus (SILVA-based
#' taxonomy), chosen so the hierarchy branches cleanly (7 phyla, 6 classes,
#' 14 orders, 18 distinct families). Species is filled only for 18S rows where the
#' source had a species-level call; `""` means unassigned.
#'
#' @format A data frame with 19 rows and 8 variables:
#' \describe{
#'   \item{marker}{`"16S"` (prokaryotes) or `"18S"` (eukaryotes)}
#'   \item{asv}{md5 hash identifying the ASV sequence}
#'   \item{Phylum, Class, Order, Family, Genus}{ranked taxonomic assignment}
#'   \item{Species}{species-level assignment (18S only), `""` when absent}
#' }
#' @source Pelagic microplastics amplicon project (derived from
#'   `DA_PELAGIC/results/tables/taxonomy_{16S,18S}.csv`; see
#'   `data-raw/build-tax_lineages.R` in the source repository).
"tax_lineages"
