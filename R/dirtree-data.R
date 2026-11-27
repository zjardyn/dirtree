#' Collapse a lineage table into tree node rows
#'
#' @description
#' `dirtree_data()` turns a table with one row per leaf (e.g. one ASV) and one
#' character column per taxonomic rank into a **node table**: one row per unique
#' path through the ranks, in depth-first order (every parent directly precedes
#' its contiguous run of children), ready to become axis breaks.
#'
#' @param x A data frame. Rank columns must be character.
#' @param ranks Character vector of column names, coarsest to finest.
#'   Defaults to the taxonomic columns present in `x`, in canonical order.
#'
#' @return A data frame with one row per tree node and columns:
#'   \item{label}{the taxon name, unprefixed}
#'   \item{level}{0-based rank depth of the node}
#'   \item{parent}{row index (`integer`) of the node's parent, `NA` for roots}
#'   \item{n}{number of leaf rows (input rows) under the node}
#'   \item{node_key}{unique axis-break key: rank-prefixed label, deduplicated
#'     across ranks (plot on this column; the guide re-adds the prefix for
#'     display, so pass the scale's raw labels)}
#'
#' @examples
#' dirtree_data(subset(tax_lineages, marker == "16S")[1:3, ])
#'
#' @export
dirtree_data <- function(x, ranks = NULL) {
  ranks <- .dirtree_ranks(x, ranks)
  paths <- .dirtree_paths(x, ranks)
  .dirtree_nodes(paths)
}

#' Resolve and validate the `ranks` argument: default to the canonical
#' taxonomic columns present in `x`; abort on non-character input and on
#' columns missing from `x` (plan validation rules, with `{?s}` pluralisation).
#' @keywords internal
#' @noRd
.dirtree_ranks <- function(x, ranks, call = rlang::caller_env()) {
  if (is.null(ranks)) {
    ranks <- intersect(
      c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
      names(x)
    )
  }
  if (!rlang::is_character(ranks) || !length(ranks)) {
    .dt_abort("{.arg ranks} must be a non-empty character vector.", call = call)
  }
  missing <- setdiff(ranks, names(x))
  if (length(missing)) {
    .dt_abort(
      c(
        "Column{?s} {.val {missing}} not found in {.arg x}.",
        i = "Rank columns are given coarsest to finest."
      ),
      call = call
    )
  }
  bad <- ranks[!vapply(x[ranks], is.character, logical(1))]
  if (length(bad)) {
    .dt_abort(
      "Rank column{?s} {.val {bad}} {?is/are} not character.",
      call = call
    )
  }
  ranks
}

# Path per input row: the rank values up to (exclusive) the first blank rank.
# Standard taxonomy truncation: a row with an unassigned Family has no usable
# Order/Family/Genus path, so it stops before the blank. The 16S source table
# has no Species column, so 16S rows stop at genus naturally.
#' Unique rank paths over the leaf rows, in first-appearance (depth-first)
#' order, with leaf counts per path.
#' @keywords internal
#' @noRd
.dirtree_paths <- function(x, ranks) {
  m <- as.matrix(x[ranks])
  blank <- is.na(m) | m == ""
  run <- t(apply(blank, 1, cumsum))
  keep <- matrix(run == 0L, nrow = nrow(m), ncol = ncol(m)) & !blank
  out <- lapply(seq_len(nrow(m)), function(i) m[i, keep[i, ], drop = TRUE])
  out[lengths(out) > 0]
}

# Node table: every UNIQUE path, including internal prefixes of leaf paths,
# in depth-first order, with parent indices and subtree leaf counts.
#
# Ordering: lexicographic order of the rank-joined path key visits a node
# before its children (a parent key is a proper prefix of its children's keys,
# and "\r" sorts below every printable character used in taxon names), with
# each node's children contiguous - i.e. depth-first.
#' Expand unique paths into the node table: one row per tree node (internal
#' ranks included), parents by nearest-preceding level - 1, leaf counts
#' accumulated up the tree.
#' @keywords internal
#' @noRd
.dirtree_nodes <- function(paths) {
  prefixes <- lapply(paths, function(p) {
    vapply(seq_along(p), function(k) paste(p[seq_len(k)], collapse = "\r"),
           character(1))
  })
  # Depth-first order by path key, with ASV leaf segments sorting LAST
  # within their parent: an "ASV.<n>_<hash>" label is a generated leaf tag,
  # not a taxon name, so it hangs at the bottom of its sibling run instead
  # of jumping alphabetically ahead of the named children. The sort key
  # prepends a high sentinel to any ASV segment; the path keys themselves
  # (and thus parent-before-child and the stored keys) are unchanged.
  keys_raw <- unlist(prefixes, use.names = FALSE)
  sort_keys <- gsub("(^|\r)asv_[0-9a-f]{8}($|\r)", "\\1\x7f\x7fasv_xxx\x7f", keys_raw)
  # method="radix" sorts byte-wise in the C locale; the default collation
  # ignores control characters and would undo the sentinel.
  all_keys <- keys_raw[order(sort_keys, method = "radix")]
  keys <- all_keys[!duplicated(all_keys)]
  split_key <- function(k) strsplit(k, "\r", fixed = TRUE)[[1]]
  label <- vapply(keys, function(k) {
    p <- split_key(k); p[length(p)]
  }, character(1))
  level <- lengths(lapply(keys, split_key)) - 1L
  parent <- vapply(seq_along(keys), function(i) {
    if (level[i] == 0L) {
      NA_integer_
    } else {
      pkey <- paste(split_key(keys[i])[seq_len(level[i])], collapse = "\r")
      match(pkey, keys)
    }
  }, integer(1))
  # n = subtree leaf count: input rows whose path passes through the node,
  # i.e. leaf keys having the node's full path key as a "\r"-delimited prefix.
  leaf_keys <- vapply(paths, function(p) paste(p, collapse = "\r"), character(1))
  n <- vapply(keys, function(k) {
    sum(leaf_keys == k | startsWith(leaf_keys, paste0(k, "\r")))
  }, integer(1))
  vctrs::data_frame(
    label = label,
    level = level,
    parent = as.integer(parent),
    n = as.integer(n),
    node_key = .dirtree_prefix_keys(label, level)
  )
}

#' Base rank prefix per level (single source for keys and display).
#' @keywords internal
#' @noRd
.dirtree_prefix_vec <- function(level, prefixes) {
  ifelse(level < length(prefixes), prefixes[level + 1L], paste0("#", level, "_"))
}

#' Unique axis key per node: rank-prefixed label, deduplicated across ranks
#' (a taxon string can name an order and a family at once, e.g.
#' Vampirovibrionales). Suffixes are ".2", ".3", ... in tree order.
#' @keywords internal
#' @noRd
.dirtree_prefix_keys <- function(label, level,
                                 prefixes = c("p_", "c_", "o_", "f_", "g_", "s_")) {
  key <- paste0(.dirtree_prefix_vec(level, prefixes), label)
  dup <- duplicated(key)
  if (any(dup)) {
    base <- key[dup]
    counts <- stats::ave(seq_along(base), base, FUN = seq_along) + 1L
    key[dup] <- paste0(base, ".", counts)
  }
  key
}
