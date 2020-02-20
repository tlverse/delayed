#' Graphical Representation of a Task Dependency Structure
#'
#' @param delayed_object the Delayed object to graph
#' @param graph the current graph, usually NULL
#' @param level the level of the node to be graphed, usually NULL
#'
#' @importFrom data.table address
#' @importFrom igraph make_empty_graph edge vertex V V<- as_data_frame
#'
#' @keywords internal
make_graph <- function(delayed_object, graph = NULL, level = 1) {
  if (is.null(graph)) {
    graph <- make_empty_graph()
  }

  uuid <- delayed_object$uuid
  arg_text <- as.character(call_args(delayed_object$expression))
  node_name <- delayed_object$name
  if (!(uuid %in% names(V(graph)))) {
    graph <- graph + vertex(
      name = uuid,
      label = node_name,
      level = level,
      sequential = delayed_object$sequential,
      state = delayed_object$state
    )

    delayed_dependencies <- delayed_object$delayed_dependencies

    # loop over dependencies
    for (i in seq_along(delayed_dependencies)) {
      graph <- make_graph(delayed_dependencies[[i]], graph, level + 1)
      arg_name <- "" # arg_text[i]
      if (is.null(arg_name)) {
        arg_name <- ""
      }
      graph <- graph + edge(delayed_dependencies[[i]]$uuid,
        uuid,
        label = arg_name
      )
    }
  } else {
    v_match <- match(uuid, names(V(graph)))
    V(graph)[[v_match]]$level <- max(V(graph)[[v_match]]$level, level)

    delayed_dependencies <- delayed_object$delayed_dependencies
    for (i in seq_along(delayed_dependencies)) {
      graph <- make_graph(delayed_dependencies[[i]], graph, level + 1)
    }
  }
  return(graph)
}

################################################################################

#' Plot Method for Delayed Objects
#'
#' @param x An object of class \code{Delayed} for which a task dependency graph
#'  will be generated.
#' @param color If \code{TRUE}, color-code nodes according to status, and
#'  display legend
#' @param height passed to visNetwork
#' @param width passed to visNetwork
#' @param ... Additional arugments (passed to visNetwork).
#'
#' @importFrom visNetwork visNetwork visEdges visHierarchicalLayout visLegend
#'  visGroups %>%
#'
#' @method plot Delayed
#'
#' @examples
#' adder <- function(x, y) {
#'   x + y
#' }
#' delayed_adder <- delayed_fun(adder)
#' z <- delayed_adder(3, 4)
#' z2 <- delayed_adder(z, 4)
#' z2$sequential <- TRUE
#' z3 <- delayed_adder(z2, z)
#' plot(z3)
#'
#' @export
plot.Delayed <- function(x, color = TRUE, height = "500px", width = "100%", ...) {
  graph <- make_graph(x)
  nodes <- as_data_frame(graph, "vertices")
  edges <- as_data_frame(graph, "edges")

  # transform nodes
  nodes <- data.frame(
    id = nodes$name, label = nodes$label, level = nodes$level,
    sequential = nodes$sequential, state = nodes$state
  )



  if (color) {
    nodes$group <- nodes$state
    nodes$shape <- ifelse(nodes$sequential, "square", "dot")
  } else {
    nodes$group <- "none"
  }

  # make graph
  network <- visNetwork(nodes, edges, height = height, width = width, ...) %>%
    visEdges(arrows = "to") %>%
    visHierarchicalLayout(direction = "RL", levelSeparation = 500,
                          nodeSpacing = 200)

  if (color) {
    # define map between state and node color
    group_states <- c("waiting", "ready", "running", "resolved", "error")
    group_colors <- c("white", "orange", "lightgreen", "black", "lightpink")
    group_colors <- lapply(group_colors, function(color) {
      list(border = "black", background = color)
    })

    # add groups definitions
    for (i in seq_along(group_states)) {
      network <- (network %>% visGroups(
        groupname = group_states[i],
        color = group_colors[[i]]
      ))
    }
    # define legend

    # legend for node colors
    legend_nodes_color <- data.frame(label = group_states)
    legend_nodes_color$color <- group_colors
    legend_nodes_color$shape <- "dot"

    # legend for node shape
    legend_nodes_shape <- data.frame(label = c("sequential", "parallel"))
    legend_nodes_shape$color <- list(
      list(border = "black", background = "white"),
      list(border = "black", background = "white")
    )
    legend_nodes_shape$shape <- c("square", "dot")

    legend_nodes <- rbind(legend_nodes_color, legend_nodes_shape)
    network <- network %>% visLegend(useGroups = FALSE,
                                     addNodes = legend_nodes)
  } else {
    network <- network %>% visGroups(
      groupname = "none",
      color = list(border = "black", background = "white")
    )
  }

  return(network)
}
