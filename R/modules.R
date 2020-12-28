#' Modules to customize the appearence of a phylogeny
#' 
#' @description **`show_modules()`** 
#' 
#' Show a data frame of the available Shiny modules.
#' 
#' @export

show_modules <- function() {
  data.frame(
    Initial = names(srv_list),
    Server = unlist(srv_list),
    UI = unlist(uis_list)
  )
}

#' @rdname show_modules
#' @description **`srv_list/uis_list`** 
#'              
#' Simple named lists with the available server and ui components.
#' These are used internally in \code{make_phynotate} to 
#' generate the application UI and server.
#' 
#' @export
# ls(asNamespace("phynotate"))[grepl("server", ls(asNamespace("phynotate")))]

srv_list <- list(
    "L" = "layout_server",
    "B" = "branches_server",
    "T" = "tips_server",
    "P" = "plotarea_server",
    "M" = "misc_server"
  )

#' @rdname show_modules
#' 
#' @export

uis_list <- list(
    "L" = "layout_ui",
    "B" = "branches_ui",
    "T" = "tips_ui",
    "P" = "plotarea_ui",
    "M" = "misc_ui"
  )

#' @rdname show_modules
#' 
#' @description **`layout_ui()/layout_server()`** 
#' 
#' Change the layout (rectangular, slanted, circular) and directon (left, right, ...) 
#' of a phylogeny displayed through the draw module. See \code{?make_phynotate} for
#' example usage. 
#' 
#' @param id namespace id
#' @param phy_data the treedata or phylo object being plotted. This is useful 
#' to extract associated data that can be used to map variables onto tree aesthetics.
#' 
#' @export

layout_ui <- function(id, phy_data) {
  ns <- NS(id)
  shiny::tagList(
    shiny::checkboxInput(
      inputId = ns("expand_layout"),
      label = "Layout",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("expand_layout"), "'] === true"),
      shiny::wellPanel(
        shiny::selectInput(
          inputId = ns("tree_layout"),
          label = "Tree layout",
          choices = c('rectangular', 'slanted', 'circular', 'fan', 'radial'),
          selected = "rectangular"
        ),
        shiny::selectInput(
          inputId = ns("tree_direction"),
          label = "Tree direction",
          choices = c('right', 'left', 'up', 'down'),
          selected = "right"
        ),
        shiny::selectInput(
          inputId = ns("ladderize"),
          label = "Ladderize",
          choices = c("right", "left"),
          selected = NULL
        )
      )
    )
  )
}

#' @rdname show_modules
#' @export
layout_server <- function(id, phy_data) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      return(reactive(list(
        "tree_layout" = input[["tree_layout"]], 
        "tree_direction" = input[["tree_direction"]],
        "ladderize" = input[["ladderize"]]
      )))
    }
  )
}

### branches

#' @rdname show_modules
#' 
#' @description **`branches_ui()/branches_server()`** 
#' 
#' Change the appearence of the branches (line color, weight) 
#' of a phylogeny displayed through the draw module. See \code{?make_phynotate} for
#' example usage.
#' 
#' @param id namespace id
#' @param phy_data the treedata or phylo object being plotted. This is useful 
#' to extract associated data that can be used to map variables onto tree aesthetics.
#'  
#' @export
branches_ui <- function(id, phy_data) {
  ns <- NS(id)
  
  phy_data <- tidytree::as_tibble(phy_data)
  if (ncol(phy_data) > 4) {
    mappable_vars <- names(phy_data[ ,-(1:4)])
  } else {
    mappable_vars <- NULL
  }

  shiny::tagList(
    shiny::checkboxInput(
      inputId = ns("expand_branches"),
      label = "Branches",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("expand_branches"), "'] === true"),
      shiny::wellPanel(
        shiny::radioButtons(
          inputId = ns("branch_size_type"),
          label = "Branch weight",
          choices = c("Variable", "Fixed"),
          selected = "Fixed", 
          inline = TRUE,
          width = "100%"
          ),
        shiny::conditionalPanel(
          condition = paste0("input['",ns("branch_size_type"),"'] === 'Fixed'"),
          shiny::numericInput(
            inputId = ns("branch_size_fixed"),
            label = "Set weight for all branches",
            min = 0,
            max = 5,
            value = 0.5,
            step = 0.1
          )
        ),
        shiny::conditionalPanel(
          condition = paste0("input['",ns("branch_size_type"),"'] === 'Variable'"),
          if (is.null(mappable_vars)) {
            helpText("Appears that the input tree does not have associated data.")
          } else {
            tagList(
          shiny::selectInput(
            inputId = ns("branch_size_variable"),
            label = "Select variable to map onto branch weight",
            choices = mappable_vars,
            multiple = FALSE,
            width = "100%"
          ),
          shiny::textInput(
            inputId = ns("branch_size_name"),
            label = "Set the name for the branch weight scale",
            width = "100%"
          ),
          shiny::sliderInput(
            inputId = ns("branch_size_limits"),
            label = "Set the limits for the branch weight scale",
            min = 0.1, 
            max = 3,
            value = c(0.3,1.3), 
            width = "100%"
          ))
          }
        ),
        colourpicker::colourInput(
          inputId = ns("branch_color"),
          label = "Color",
          value = "grey45"
        )
      )
    )
  )
}

#' @rdname show_modules
#' @export
branches_server <- function(id, phy_data) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      
      return(reactive(list(
        "branch_size_type" = input[["branch_size_type"]],
        "branch_size_fixed" = input[["branch_size_fixed"]],
        "branch_size_variable" = input[["branch_size_variable"]],
        "branch_size_name" = input[["branch_size_name"]],
        "branch_size_limits" = input[["branch_size_limits"]],
        
        "branch_color" = input[["branch_color"]]
      )))
    }
  )
}

### tips

#' @rdname show_modules
#' 
#' @description **`tips_ui()/tips_server()`** 
#' 
#' Enable and change the appearence of the tip labels (font color, font size, offset) 
#' of a phylogeny displayed through the draw module. See \code{?make_phynotate} for
#' example usage.
#' 
#' @param id namespace id
#' @param phy_data the treedata or phylo object being plotted. This is useful 
#' to extract associated data that can be used to map variables onto tree aesthetics.
#' 
#' @export
tips_ui <- function(id, phy_data) {
  ns <- NS(id)
  shiny::tagList(
    shiny::checkboxInput(
      inputId = ns("expand_tips"),
      label = "Tips",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("expand_tips"), "'] === true"),
      shiny::wellPanel(
        shiny::helpText("Likely illegible for large trees"),
        shiny::checkboxInput(
          inputId = ns("show_tip_labels"),
          label = "Show tip labels",
          value = FALSE
        ),
        shiny::numericInput(
          inputId = ns("tip_font_size"),
          label = "Font size",
          min = 1,
          max = 20,
          value = 5,
          step = 0.5
        ),
        shiny::numericInput(
          inputId = ns("tip_label_offset"),
          label = "Offset",
          min = 0,
          max = 10,
          value = 0.2,
          step = 0.1
        ),
        colourpicker::colourInput(
          inputId = ns("tip_color"),
          label = "Color",
          value = "grey45"
        ),
        shiny::numericInput(
          inputId = ns("right_pad"),
          label = "Right side padding",
          min = 0,
          max = 10,
          value = 0.5,
          step = 0.1
        )
      )
    )
  )
}

#' @rdname show_modules
#' @export
tips_server <- function(id, phy_data) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      return(reactive(list(
        "show_tip_labels" = input[["show_tip_labels"]], 
        "tip_font_size" = input[["tip_font_size"]],
        "tip_label_offset" = input[["tip_label_offset"]],
        "tip_color" = input[["tip_color"]],
        "right_pad" = input[["right_pad"]]
      )))
    }
  )
}

### plot area

#' @rdname show_modules
#' 
#' @description **`plotarea_ui()/plotarea_server()`** 
#' 
#' Change the plotting areaof a phylogeny displayed through the draw module. 
#' For now, only plot height and width can be modified. See \code{?make_phynotate} for
#' example usage.
#' 
#' @param id namespace id
#' @param phy_data the treedata or phylo object being plotted. This is useful 
#' to extract associated data that can be used to map variables onto tree aesthetics.
#' 
#' @export
plotarea_ui <- function(id, phy_data) {
  ns <- NS(id)
  shiny::tagList(
    shiny::checkboxInput(
      inputId = ns("expand_resize"),
      label = "Plot area",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("expand_resize"), "'] === true"),
      shiny::wellPanel(
        shiny::sliderInput(
          inputId = ns("height"),
          label = "Plot height",
          min = 100,
          max = 1000,
          value = 600
        ),
        shiny::sliderInput(
          inputId = ns("width"),
          label = "Plot width",
          min = 100,
          max = 1000,
          value = 400
        )
      )
    )
  )
}
#' @rdname show_modules
#' @export
plotarea_server <- function(id, phy_data) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      return(reactive(list(
        "height" = input[["height"]], 
        "width" = input[["width"]]
      )))
    }
  )
}

### misc

#' @rdname show_modules
#' 
#' @description **`misc_ui()/misc_server()`** 
#' 
#' Less common customizations of a phylogeny displayed through the draw module. 
#' For example, the to modify the amount of space (degrees) between the first and last tip
#' of a phylogeny plotted in circular layout. See \code{?make_phynotate} for
#' example usage.
#' 
#' @param id namespace id
#' @param phy_data the treedata or phylo object being plotted. This is useful 
#' to extract associated data that can be used to map variables onto tree aesthetics.
#' 
#' @export
misc_ui <- function(id, phy_data) {
  ns <- NS(id)
  shiny::tagList(
    shiny::checkboxInput(
      inputId = ns("expand_misc"),
      label = "Miscellaneous",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("expand_misc"), "'] === true"),
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
    )
  )
}

#' @rdname show_modules
#' @export
misc_server <- function(id, phy_data) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      return(reactive(list(
        "open_angle" = input[["open_angle"]]
      )))
    }
  )
}