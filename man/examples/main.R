  if (interactive()) {
  library(ape)
  data("bird.orders")
  ui <- fluidPage(
    titlePanel("Shiny module integrating drawing and annotation modules"),
    mainPanel(
    main_ui(id = "id", modules = "LPTB")
    )
  )
  
  server <- function(input, output, session) {
    main_server(id = "id", phylogeny = bird.orders, modules = "LPTB")
  }
  
  shinyApp(ui = ui, server = server)
}