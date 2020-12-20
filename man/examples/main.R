if (interactive()) {
  library(shiny)
  library(phynotate)
  library(ape)
  
  ui <- shiny::navbarPage(
    title = "A shiny app where we need to include a page for phylogeny viz",
    tabPanel(title = "Download sequences"),
    tabPanel(title = "Infer orthology"),
    tabPanel(
      title = "Vizualize and download trees",
      phynotate::main_ui(
        id = "id",
        draw_modules = "LPTB",
        anno_module = FALSE
      )
    )
  )
  
  server <- function(input, output, session) {
    # Server code to download sequences
    # Server code to infer orthology
    
    # Send a user selected tree to the phynotate viz module
    main_server(id = "id",
                phylogeny = ape::rcoal(20),
                #phylogeny = input[["user_selected_tree"]],
                draw_modules = "LPTB")
  }
  
  shinyApp(ui = ui, server = server)
}