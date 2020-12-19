#' Make a Shiny application to display, customize appearence, and annotate a phylogeny
#'
#' @description `make_phynotate` generates a standalone `{shiny}` app with one phylogeny and
#'              widgets for customizing its appearence. This is a wrapper around
#'              `phynotate::main_ui/server` to simplify usage when a the main module is
#'              called as a standalone app. See `?draw_server` and `?main_server` for an 
#'              for info on how to include the modules in an existing app.
#'
#' @param phylogeny an object of class `phylo` to display. Very large phylogenies
#'                  typically will be dificult to display legibly.
#' @param draw_modules character string specifying the customizing widgets (shiny modules) to
#'                include. The default is `"LPBT"` which stands for Layout, Plot area,
#'                Branches, and Tips. Changing the content and order can be achieved by
#'                changing the string, e.g., `"LP"` will only allow customization
#'                of the plot layout and size, whereas `"BTL"` will show widgets for
#'                branches, tips, and layout in that order.
#' @param annotation_module logical. Whether to include the 'clade_annotation' module (`annotate_ui/server`)
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

make_phynotate <- function(phylogeny = NULL, draw_modules = "LPTB", annotation_module = FALSE) {
  ui <-
    fluidPage(
      titlePanel("Shiny module to draw phylogenetic trees"),
      main_ui(
        id = "main",
        draw_modules = draw_modules,
        anno_module = annotation_module
      )
    )
  
  server <- function(input, output, session) {
    main_server(
      id = "main",
      phylogeny = phylogeny,
      draw_modules = draw_modules,
      anno_module = annotation_module
    )
  }
  
  shinyApp(ui = ui, server = server)
}
