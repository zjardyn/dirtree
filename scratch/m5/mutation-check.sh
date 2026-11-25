#!/bin/sh
# Mutation spot-checks demanded by the drift sentinel post-pass.
# Each mutant must redden the suite; survivors mean the contract is unguarded.
set -e
ROOT=$(cd "$(dirname "$0")" && cd .. && cd .. && pwd -P)
cd "$ROOT"
# Refuse to run with a dirty R/: run() restores R/ from HEAD, which would
# silently revert uncommitted work (this destroyed a fix in d86cfba).
git diff --quiet R/ || { echo "R/ is dirty — commit before mutating"; exit 1; }
run() { # $1 = label
  out=$(Rscript -e 'r <- as.data.frame(devtools::test(reporter="silent", stop_on_failure=FALSE)); cat(sprintf("%dF %dE %dP", sum(r$failed), sum(r$error), sum(r$passed)))' 2>&1 | tail -1)
  echo "$1 -> $out"
  git checkout -- R/
}

# M1: guide x N -> leaf-only (the overturned rule)
sed -i 's/mult <- ifelse(n > 1L, paste0(" \\u00d7 ", n), "")/mult <- ifelse(n > 1L \& levels == max(levels), paste0(" \\u00d7 ", n), "")/' R/guide.R
grep -q "max(levels)" R/guide.R && run "M1 guide x-N leaf-only"

# M2: root trunk branch removed (forest rule dropped)
sed -i 's/if (any(!kid)) {/if (FALSE) {/' R/tree.R
grep -q "if (FALSE) {" R/tree.R && run "M2 root-trunk removed"

# M3: nodes = NULL guard removed
sed -i 's/if (is.null(nodes)) {/if (FALSE) {/' R/guide.R
grep -q "if (FALSE) {" R/guide.R && run "M3 nodes-guard removed"

# M4: guide forces vertical=TRUE (orientation forwarding broken toward y)
sed -i 's/vertical = isTRUE(params\\\$vertical)/vertical = TRUE/' R/guide.R
grep -q "vertical = TRUE$" R/guide.R && run "M4 guide vertical forced TRUE"
echo "done"
