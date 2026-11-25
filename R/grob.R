# grid rendering of a dirtree layout (see R/tree.R for the geometry and its
# lineage to template/dirtree.dtx).

dirtree_grob <- function(nodes, labels, gp = NULL, gp_comment = NULL,
                        geom = dirtree_geom(), vertical = FALSE) {
  .check_dirtree_inputs(nodes, labels)
  ly <- dirtree_layout(nodes, geom)
  if (is.null(ly)) {
    return(grid::grid.null())
  }
  ticks <- with(ly, grid::segmentsGrob(
    x0 = x(ly$tick$x0), y0 = y(ly$tick$y0),
    x1 = x(ly$tick$x1), y1 = y(ly$tick$y1),
    gp = rule_gp(gp)
  ))
  trunk <- with(ly, grid::segmentsGrob(
    x = x(ly$trunk$x), y0 = y(ly$trunk$y0), y1 = y(ly$trunk$y1),
    gp = rule_gp(gp)
  ))
  root <- if (is.null(ly$root_trunk)) {
    grid::nullGrob()
  } else with(ly, grid::segmentsGrob(
    x = x(root_trunk$x), y0 = y(root_trunk$y0), y1 = y(root_trunk$y1),
    gp = rule_gp(gp)
  ))
  txt <- grid::textGrob(
    label = labels,
    x = x(ly$x_label), y = y(ly$y),
    hjust = if (vertical) 0 else 0.5,
    vjust = if (vertical) 0.5 else 1,
    gp = gp
  )
  grid::gTree(
    children = grid::gList(root, trunk, ticks, txt),
    vp = if (vertical) {
      grid::viewport(width = grid::unit(ly$width %||% 0, "in"),
                     height = grid::unit(ly$height, "in"),
                     x = grid::unit(0, "npc"), y = grid::unit(0.5, "npc"),
                     just = c("left", "center"), name = "dirtree")
    } else {
      grid::viewport(height = grid::unit(ly$height, "in"),
                     x = grid::unit(0.5, "npc"), y = grid::unit(1, "npc"),
                     just = c("center", "top"), name = "dirtree")
    },
    name = "dirtree-grob"
  )
}

# Boundary validation for direct dirtree_grob() calls (the guide validates the
# key/levels alignment at its own slice site; this guards the ggplot-free core).
.check_dirtree_inputs <- function(nodes, labels) {
  need <- c("label", "level", "parent", "n")
  if (!all(need %in% names(nodes))) {
    rlang::abort(c(
      "{.arg nodes} must be a node table with columns {.val {need}}.",
      i = "Build one with {.fn dirtree_data}."
    ))
  }
  if (length(labels) != nrow(nodes)) {
    rlang::abort(
      "{.arg labels} has length [{length(labels)}], not the [{nrow(nodes)}] node rows."
    )
  }
  invisible(TRUE)
}

rule_gp <- function(gp) {
  g <- grid::gpar(linewidth = 0.4, lineend = "butt", linejoin = "mitre")
  if (!is.null(gp)) {
    common <- intersect(names(gp), names(g))
    g[common] <- gp[common]
    g$col <- gp$col %||% gp$colour %||% g$col
  }
  g
}

x <- function(v) grid::unit(v, "in")
y <- function(v) grid::unit(v, "in")
