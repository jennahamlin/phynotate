if (interactive()) {

  ui <- fluidPage(
    titlePanel("Shiny module to draw phylogenetic trees"),
    mainPanel(
    draw_ui(id = "id")
    )
  )
  
  server <- function(input, output, session) {
    X <- ape::rtree(150)
    draw_server(id = "id", phylogeny = X)
  }
  
  shinyApp(ui = ui, server = server)
}

