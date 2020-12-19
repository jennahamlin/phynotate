#' Shiny module to draw a phylogeny
#'
#' @param id character, namespace id
#' @param phylogeny object of class `phylo` to draw
#' @param modules modules to include in the tree-drawing application (or page). 
#'                See `?show_modules` for available options
#'
#' @return UI and server logic for plotting and interactive modification of a phylogeny
#'
#' @details Trees with > 200 tips will in most cases be illegible. Tip names become illegible in smaller trees.
#'
#' @example /man/examples/draw.R
#'
#' @export
draw_server <- function(id, phylogeny, modules = "L") {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      react_list <- reactiveValues()
      
      # call the server functions of all required modules
      R <- lapply(strsplit(modules, "")[[1]], function(x) {
        rlang::exec(srv_list[[x]], list(id = x))
      })
      
      # listen to user changes in module UIs 
      observe({
        RR <- lapply(1:length(R), function(x) R[[x]]())
        RR <- unlist(RR, recursive = FALSE)
        
        lapply(names(RR), function(i) {
          react_list[[i]] <- RR[[i]]
        })
      })
      
      return(react_list)
    }
  )
}

#' @rdname draw_server
#'
#' @description Module UI
#'
#' @export
draw_ui <- function(id, modules = "L") {
  ns <- shiny::NS(id)
  # shiny::fluidRow(
      tagList(lapply(strsplit(modules, "")[[1]], function(x) {
        rlang::exec(uis_list[[x]], list(id = ns(x)))
      }))
    # )
}

param_list <- list(
  phylogeny = NULL,
  show_tip_labels = FALSE,
  tip_font_size = 5,
  tip_label_offset = 1,
  tree_layout = "rectangular",
  tree_direction = "right",
  time_axis_ticks = 10,
  open_angle = 10,
  branch_size = 0.5,
  branch_color = "grey45",
  tip_color = "grey45",
  width = 200,
  height = 500
)

draw_tree <- function(tree, par_list = param_list) {
  if (!par_list[["tree_layout"]] %in% c('rectangular', 'circular', 'slanted', 'fan', 'radial')) {
    stop("The selected tree layout is not supported.")
  }
  
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

add_tips <- function(g, size, color, offset, rotation, justification) {
  plot_data <- g$data[g$data$isTip == TRUE,]
  g + geom_text(
    data = plot_data,
    aes(
      x = .data$x + offset,
      y = .data$y,
      label = .data$label
    ),
    size = size,
    color = color,
    angle = rotation,
    hjust = justification
  )
}

tree_flip <- function(g, par_list = param_list) {
  rot <- 0 # tip label rotation
  just <- 0 # tip label justification 
  
  if (par_list[["show_tip_labels"]]) {
    pad <- c(0.2, par_list[["right_pad"]])
  } else {
    pad <- c(0.2, 0.2)
  }
  
  if (par_list[["tree_direction"]] %in% c("left", "down")) {
    pad <- rev(pad)
  }
  
  if (par_list[["tree_layout"]] %in% c("rectangular", "slanted")) {
    if (par_list[["tree_direction"]] == "up") {
      g <- g + 
        ggplot2::coord_flip() +
        scale_x_continuous(expand = expansion(mult = pad))
      rot <- 90
    }
    
    if (par_list[["tree_direction"]] == "down") {
      g <- g +
        ggplot2::coord_flip() +
        ggplot2::scale_x_reverse(expand = expansion(mult = pad))
      rot <- 270
    }
    
    if (par_list[["tree_direction"]] == "left") {
      g <- g + 
        ggplot2::scale_x_reverse(expand = expansion(mult = pad))
      just <- 1
    }
    
    if (par_list[["tree_direction"]] == "right") {
      g <- g + 
        scale_x_continuous(expand = expansion(mult = pad))
    }
    
    if (par_list[["show_tip_labels"]] == TRUE) {
      # g <- add_tips(
      #   g = g,
      #   size = par_list[["tip_font_size"]],
      #   color = par_list[["tip_color"]],
      #   offset = par_list[["tip_label_offset"]],
      #   rotation = rot,
      #   justification = just
      # )
      
      g <- g + ggtree::geom_tiplab(
        geom = "text",
        size = par_list[["tip_font_size"]],
        color = par_list[["tip_color"]],
        offset = par_list[["tip_label_offset"]],
        angle = rot,
        hjust = just
      )
    }
  }
  
  g <- g +
    ggplot2::theme(axis.text = ggplot2::element_blank()) +
    ggplot2::theme(axis.line = ggplot2::element_blank()) +
    ggplot2::theme(axis.title = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", size = 1))
  return(g)
}
