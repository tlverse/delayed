#' Animated Representation a Task Dependency Structure
#'
#' @description uses shiny
#'
#' @param scheduler the scheduler to animate
#'
#' @import visNetwork
#'
#' @examples
#' \donttest{
#' adder <- function(x, y) {
#'   x + y
#' }
#' delayed_adder <- delayed_fun(adder)
#' z <- delayed_adder(3, 4)
#' z2 <- delayed_adder(z, 4)
#' z2$sequential <- TRUE
#' z3 <- delayed_adder(z2, z)
#' plot_delayed_shiny(z3)
#' }
#'
#' @export
plot_delayed_shiny <- function(scheduler) {
  requirePackages("shiny")

  delayed_object <- scheduler$delayed_object

  server <- function(input, output, session) {
    network <- plot.Delayed(delayed_object)

    running <- FALSE
    output$network <- renderVisNetwork({
      network
    })
    shiny::observeEvent(input$start, {
      running <<- !running
      if (delayed_object$resolved) {
        running <<- FALSE
      }
    })

    shiny::observe({
      shiny::invalidateLater(100, session)
      if (running) {
        updated <- scheduler$compute_step()
      } else {
        updated <- c()
      }

      if (length(updated) > 0) {
        addresses <- sapply(updated, `[[`, "uuid")
        node_states <- sapply(updated, `[[`, "state")
        nodes_df <- data.frame(id = addresses, group = node_states)
        visNetworkProxy("network") %>%
          visUpdateNodes(nodes_df)

        if (delayed_object$resolved) {
          running <<- FALSE
          message("Delayed is now resolved")
        }
      }
    })
  }

  ui <- shiny::fluidPage(
    shiny::actionButton("start", "Click to start/pause computation"),
    visNetworkOutput("network")
  )
  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app)
}
