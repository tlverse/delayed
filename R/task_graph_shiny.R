#' Animated Representation a Task Dependency Structure
#' @description uses shiny
#' @param scheduler the scheduler to animate
#' @import visNetwork
#' @export
#
plot_delayed_shiny <- function(scheduler){
  requirePackages("shiny")

  delayed_object <- scheduler$delayed_object
  
  server <- function(input, output, session) {
    network <- plot.Delayed(delayed_object, height="100%", width="100%")

    running <- FALSE
    output$network <- renderVisNetwork({
      network
    })
    shiny::observeEvent(input$start, {
      running <<- !running
      if(delayed_object$resolved){
        running <<- FALSE
      }
    })
    
    shiny::observe({
      
      shiny::invalidateLater(100, session)
      if(running){
        updated <- scheduler$compute_step()
      } else {
        updated <- c()
      }
      
      if(length(updated)>0){
        addresses <- sapply(updated,`[[`, "uuid")
        node_states <- sapply(updated, `[[`, "state")
        nodes_df <- data.frame(id=addresses, group=node_states)
        visNetworkProxy("network") %>%
          visUpdateNodes(nodes_df)
        
        if(delayed_object$resolved){
          running <<- FALSE
          message("Delayed is now resolved")
        }
      }    
      
    })
    
  }
  
  ui <- shiny::fluidPage(
    shiny::actionButton("start", "Click to start/pause computation"),
    visNetworkOutput("network", height="800px")
  )
  
  app <- shiny::shinyApp(ui = ui, server = server)
  
  shiny::runApp(app)
}