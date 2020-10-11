#' Shiny module to draw a phylogeny
#'
#' @param id character, namespace id
#' @param phylogeny object of class `phylo` to draw
#'
#' @return UI and server logic for plotting and interactive modification of a phylogeny
#'
#' @details Trees with > 200 tips will in most cases be illegible. Tip names become illegible in smaller trees.
#'
#' @example /man/examples/draw.R
#'
#' @export
draw_server <- function(id, phylogeny) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      react_list <- reactiveValues()
      lapply(names(param_list), function(x)
        react_list[[x]] <- param_list[[x]])
      
      observe({
        lapply(names(input), function(x)
          react_list[[x]] <- input[[x]])
      })
      
      observeEvent(input$clear_formatting, {
        lapply(names(param_list), function(x)
          react_list[[x]] <- param_list[[x]])
        shinyjs::reset("control_panel")
      })
      
      output$phy_plot <- shiny::renderPlot({
        draw_tree(tree = phylogeny, par_list = react_list)
      })
    }
  )
}

#' @rdname draw_srv
#'
#' @description Module UI
#' @param id
#'
#' @export
draw_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::fluidRow(
    shiny::column(
      width = 3,
      shinyjs::useShinyjs(),
      id = ns("control_panel"),
      # layout
      shinyWidgets::prettyCheckbox(
        inputId = "expand_layout",
        label = "Layout",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = "input.expand_layout == true",
        shiny::wellPanel(
          shiny::selectInput(
            inputId = ns("tree_layout"),
            label = "Tree layout",
            choices = c('rectangular', 'slanted', 'circular', 'fan', 'radial'),
            selected = "slanted"
          ),
          shiny::selectInput(
            inputId = ns("tree_direction"),
            label = "Tree direction",
            choices = c('right', 'left', 'up', 'down'),
            selected = "right"
          )
        )
      ),
      # branches
      shinyWidgets::prettyCheckbox(
        inputId = "expand_branches",
        label = "Branches",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = "input.expand_branches == true",
        shiny::wellPanel(
          shiny::numericInput(
            inputId = ns("branch_size"),
            label = "Branch line weight",
            min = 0,
            max = 5,
            value = 0.5,
            step = 0.5
          ),
          colourpicker::colourInput(
            inputId = ns("branch_color"),
            label = "Color",
            value = "grey45"
          )
        )
      ),
      # tips
      shinyWidgets::prettyCheckbox(
        inputId = "expand_tips",
        label = "Tips",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = "input.expand_tips == true",
        shiny::wellPanel(
          shiny::helpText("Likely illegible for large trees"),
          shinyWidgets::prettyCheckbox(
            inputId = ns("show_tip_labels"),
            label = "Show tip labels",
            value = FALSE
          ),
          shiny::numericInput(
            inputId = ns("tip_font_size"),
            label = "Font size",
            min = 5,
            max = 20,
            value = 5,
            step = 1
          ),
          shiny::numericInput(
            inputId = ns("tip_label_offset"),
            label = "Offset",
            min = 0,
            max = 5,
            value = 1,
            step = 0.01
          ),
          colourpicker::colourInput(
            inputId = ns("tip_color"),
            label = "Color",
            value = "grey45"
          )
        )
      ),
      
      # additionals
      shinyWidgets::prettyCheckbox(
        inputId = "expand_misc",
        label = "Miscellaneous",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = "input.expand_misc == true",
        shiny::wellPanel(
          shiny::numericInput(
            inputId = ns("open_angle"),
            label = "Open angle",
            min = 0,
            max = 360,
            value = 10,
            step = 5
          ),
          shiny::helpText(
            "Space in degrees between the first and last tip when tree layout is 'fan'. Useful for drawing semi-circular or pizza slice trees."
          )
        )
      ),
      shiny::actionButton(inputId = ns("clear_formatting"), label = "Clear formatting"),
    ),
    shiny::column(width = 6, shiny::plotOutput(outputId = ns("phy_plot")))
  ))
}

param_list <- list(
  show_tip_labels = FALSE,
  tip_font_size = 5,
  tip_label_offset = 1,
  tree_layout = "rectangular",
  tree_direction = "right",
  time_axis_ticks = 10,
  open_angle = 10,
  branch_size = 0.5,
  branch_color = "grey45",
  tip_color = "grey45"
)

draw_tree <- function(tree, par_list = param_list) {
  if (!par_list[["tree_layout"]] %in% c('rectangular', 'circular', 'slanted', 'fan', 'radial')) {
    stop("The selected tree layout is not supported.")
  }
  
  agemax <- tree %>% ape::branching.times() %>% max()
  
  g <-
    ggtree::ggtree(
      tr = tree,
      layout = par_list[["tree_layout"]],
      size = par_list[["branch_size"]],
      color = par_list[["branch_color"]],
      open.angle = par_list[["open_angle"]]
    )
  
  tree_flip(g = g, par_list = par_list)
}

add_tips <- function(g, size, color, offset) {
  plot_data <- g$data[g$data$isTip == TRUE,]
  g + geom_text(
    data = plot_data,
    aes(
      x = .data$x + offset,
      y = .data$y,
      label = .data$label
    ),
    size = size,
    color = color
  )
}

tree_flip <- function(g, par_list = param_list) {
  if (par_list[["tree_layout"]] %in% c("rectangular", "slanted")) {
    if (par_list[["tree_direction"]] == "up") {
      g <- g + ggplot2::coord_flip()
    }
    
    if (par_list[["tree_direction"]] == "down") {
      g <- g +
        ggplot2::coord_flip() +
        ggplot2::scale_x_reverse()
    }
    
    if (par_list[["tree_direction"]] == "left") {
      g <- g + ggplot2::scale_x_reverse()
    }
    
    if (par_list[["show_tip_labels"]] == TRUE) {
      g <- add_tips(
        g = g,
        size = par_list[["tip_font_size"]],
        color = par_list[["tip_color"]],
        offset = par_list[["tip_label_offset"]]
      )
    }
  }
  
  g <- g +
    ggplot2::theme(axis.text = ggplot2::element_blank()) +
    ggplot2::theme(axis.line = ggplot2::element_blank()) +
    ggplot2::theme(axis.title = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", size = 1)) +
    ggplot2::theme(plot.margin = unit(rep(.1, 4), "in"))
  
  return(g)
}
