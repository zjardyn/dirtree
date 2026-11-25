# dirtree R dev container: rocker/geospatial (R 4.6.2, ggplot2 4.x, devtools,
# testthat, roxygen2, pkgload, svglite) + the remaining CRAN gate tools.
# Build (from repo root):
#   docker build --platform linux/arm64 -t dirtree-dev .dev
# Run (host uid, repo mounted at same path):
#   docker run --rm -u "$(id -u):$(id -g)" -e HOME=/tmp -v "$PWD:$PWD" -w "$PWD" \
#     dirtree-dev Rscript <script>
FROM --platform=linux/arm64 rocker/geospatial:latest

RUN install2.r --error --skipmissing --skipinstalled -r https://cloud.r-project.org \
      styler spelling urlchecker pkgdown \
 && rm -rf /tmp/downloaded_packages
