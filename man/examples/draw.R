  if (interactive()) {
  library(ape)
  data("bird.orders")
  ui <- fluidPage(
    titlePanel("Shiny module to draw phylogenetic trees"),
    mainPanel(
    draw_ui(id = "id", modules = "LPTB")
    )
  )
  
  server <- function(input, output, session) {
    draw_server(id = "id", phylogeny = bird.orders, modules = "LPTB")
  }
  
  shinyApp(ui = ui, server = server)
}