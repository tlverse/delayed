#' Graphical Representation of a Task Dependency Structure
#'
#' @param delayed_object the Delayed object to graph
#' @param graph the current graph, usually NULL
#' @param level the level of the node to be graphed, usually NULL
#'
#' @importFrom data.table address
#' @importFrom igraph make_empty_graph edge vertex V V<- as_data_frame
#'
#' @export
#
make_graph <- function(delayed_object, graph=NULL, level=1) {
  if (is.null(graph)) {
    graph <- make_empty_graph()
  }

  my_address <- data.table::address(delayed_object)
  arg_text <- as.character(lang_tail(UQ(delayed_object$expression)))
  node_name <- delayed_object$name
  if (!(my_address%in%names(V(graph)))) {
    graph <- graph + vertex(my_address,
                            shape = "rectangle",
                            label = node_name,
                            level = level,
                            sequential = delayed_object$sequential,
                            state = delayed_object$state)

    delayed_dependencies <-  delayed_object$delayed_dependencies

    # loop over dependencies
    for (i in seq_along(delayed_dependencies)) {
      graph <- make_graph(delayed_dependencies[[i]], graph, level + 1)
      arg_name <- ""#arg_text[i]
      if (is.null(arg_name)) {
        arg_name <- ""
      }
      graph <- graph + edge(data.table::address(delayed_dependencies[[i]]),
                            my_address, label = arg_name)
    }

  } else {
    v_match = match(my_address, names(V(graph)))
    V(graph)[[v_match]]$level <- max(V(graph)[[v_match]]$level, level)

    delayed_dependencies <-  delayed_object$delayed_dependencies
    for (i in seq_along(delayed_dependencies)) {
      graph <- make_graph(delayed_dependencies[[i]], graph, level + 1)
    }
  }
  return(graph)
}

#' Plot Method for Delayed Objects
#'
#' @importFrom visNetwork visNetwork visEdges visHierarchicalLayout %>%
#'
#' @export
#
plot.Delayed <- function(x, ...) {
  graph <- make_graph(x)
  nodes <- as_data_frame(graph, "vertices")
  nodes <- data.frame(id = nodes$name, label = nodes$label, level = nodes$level,
                      sequential = nodes$sequential, state = nodes$state)
  nodes$color <- c("blue", "red", "orange",
                   "green")[match(nodes$state,
                            c("waiting","ready","running","resolved"))]
  # nodes$level = c(1,4,5,2,3,3)
  edges <- as_data_frame(graph, "edges")
  visNetwork(nodes, edges, width = "100%") %>% 
    visEdges(arrows = "to") %>% 
    visHierarchicalLayout(direction = "RL", levelSeparation = 500)
}

