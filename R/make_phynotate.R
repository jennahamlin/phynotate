#' Make a Shiny application to display, customize appearence, and annotate a phylogeny
#' 
#' @description \code{make_phynotate} generates a standalone Shiny app with one phylogeny and 
#'              widgets for customizing its appearence. This is a wrapper around
#'              \code{phynotate::draw_ui/server} to simplify usage when a the draw module is 
#'              called as a standalone app. See `?draw_server` for an example of how to include 
#'              the module in an existing app.
#'                
#' @param phylogeny an object of class \code{phylo} to display. Very large phylogenies 
#'                  typically will be dificult to display legibly. 
#' @param modules character string specifying the customizing widgets (shiny modules) to
#'                include. The default is \code{"LPBT"} which stands for Layout, Plot area, 
#'                Branches, and Tips. Changing the content and order can be achieved by
#'                changing the string, e.g., \code{"LP"} will only allow customization 
#'                of the plot layout and size, whereas \code{"BTL"} will show widgets for
#'                branches, tips, and layout in that order.
#'                
#' See \code{?modules} for details about the modules.
#' 
#' @examples 
#' if (interactive()) {
#'    library(ape)
#'    data(bird.orders)
#'    make_phynotate(phylogeny=bird.orders, modules = "LPBT") 
#' }
#' 
#' @export

make_phynotate <- function(phylogeny, modules = "LPTB") {
  ui <- fluidPage(titlePanel("Shiny module to draw phylogenetic trees"),
                  mainPanel(draw_ui(id = "id", modules = modules)))
  
  server <- function(input, output, session) {
    draw_server(id = "id",
                phylogeny = phylogeny,
                modules = modules)
  }
  
  shinyApp(ui = ui, server = server)
}
