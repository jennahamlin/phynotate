#' Shiny outer module to integrate draw and annotation inner modules 
#'
#' @param id character, namespace id
#' @param phylogeny object of class `phylo` to draw
#' @param draw_modules character. Modules to include in the tree-drawing application (or page). 
#' The modules are specified through their Initial as given in `show_modules()`. See `?show_modules` 
#' for available options. 
#' @param anno_module logical. Whether to include the 'clade_annotation' module (`annotate_ui/server`)
#'
#' @return UI and server logic for plotting and interactive modification of a phylogeny
#'
#' @details Trees with > 200 tips will in most cases be illegible. Tip names become illegible in smaller trees.
#'
#' @example /man/examples/main.R
#'
#' @export

main_server <- function(id, phylogeny, draw_modules, anno_module = FALSE) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      validate(need(expr = !is.null(phylogeny), 
                    message = "Please supply a phylogeny.")
      )
      
      # create and populate a list of reactive values used to draw the phylogeny
      react_list <- reactiveValues()
      lapply(names(param_list), function(x) {
        # default values to be updated later
        react_list[[x]] <- param_list[[x]]
      })
      
      # listen to user input from the tree
      observe({
        react_list[["phylogeny"]] <- phylogeny
      })
      
      # make the plot object to be updated by annotation module
      observe({
        react_list[["tree_plot"]] <-
          draw_tree(tree = react_list[["phylogeny"]], par_list = react_list)
      })
      
      # render the plot with reactive dimensions
      output$phy_plot <- shiny::renderPlot(
        width = function() react_list[["width"]],
        height = function() react_list[["height"]],
        res = 96,
        {
          react_list[["tree_plot"]]
        })
      
      observe({
        # print("brushed: ", input[["phy_brush"]])
        react_list[["phy_brush"]] <- input[["phy_brush"]]
      })
      
      # call the customizing modules which will update the react list
      draw_inputs <- draw_server(id = "draw", modules = draw_modules)
      
      if (anno_module) {
        anno_inputs <- annotate_server(
          id = "anno", 
          react_list = react_list,
          plot_inputs = reactive(input[["phy_brush"]])
        )
      }
      
      # update the react list with user actions from inner modules
      observe({
        lapply(names(draw_inputs), function(i) {
          react_list[[i]] <- draw_inputs[[i]]
        })
        
        if (anno_module) {
          lapply(names(anno_inputs), function(i) {
            react_list[[i]] <- anno_inputs[[i]]
          })
        }
      })
      
      # listen to user click to clear the formatting
      observeEvent(input$clear_formatting, {
        lapply(names(param_list), function(x) {
          react_list[[x]] <- param_list[[x]]
          react_list[["phylogeny"]] <- phylogeny
        })
        shinyjs::reset("control_panel")
      })
    }
  )
}

#' @rdname main_server
#' @export

main_ui <- function(id, phylogeny, draw_modules, anno_module = FALSE) {
  ns <- NS(id)
  shiny::tagList(
    shiny::column(
      width = 3,
      shinyjs::useShinyjs(),
      id = ns("control_panel"),
      draw_ui(id = ns("draw"), phylogeny = phylogeny, modules = draw_modules),
      shiny::actionButton(inputId = ns("clear_formatting"), label = "Clear formatting")
    ),
    shiny::column(width = 6, shiny::plotOutput(
      brush = ns("phy_brush"),
      outputId = ns("phy_plot")
    )),
    shiny::column(width = 3,
                  {
                    if (anno_module) {
                      annotate_ui(id = ns("anno"))
                    }
                  })
  )
}