#' Annotate clades with a name or color 
#' 
#' @description 
#' @param id namespace id
#' @param plot_inputs `reactive`. A list of inputs from an interactive plot. For now only plot brush (see `?shiny::plotOutput`) is handled.
#' @param react_list `reaciveVal`. A list of variables updated within the app and passed between modules. 
#' @export

annotate_server <- function(id, react_list, plot_inputs) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      
      annotations_table <- reactiveVal()
      at <- data.frame("name" = NULL, "offset" = NULL)
      annotations_table(at)
      
      #display the layer onto the tree
      observeEvent(input$add_anno, {
        # update the number of annotations
        react_list[["n_anno"]] <- input$add_anno
        
        selected_tips <-
          brushedPoints(react_list[["tree_plot"]]$data, plot_inputs())
        can_label <- reactiveVal(TRUE)
        
        if (nrow(selected_tips) == 0) {
          can_label(FALSE)
          showNotification(type = "error",
                           ui = "Please make sure the brushed area overlaps a terminal branch of the phylogeny. (Not only tip labels.)")
        }
        
        if (nrow(selected_tips) > 0 & sum(selected_tips$isTip) == 0) {
          can_label(FALSE)
          showNotification(type = "error",
                           ui = "Please make sure the brushed area overlaps a termninal branch of the phylogeny. (Not only internal nodes.")
        }
        
        #add label to tipVector if isTip == True
        req(plot_inputs())
        
        if (can_label()) {
          
          tip_vector <- selected_tips$label[selected_tips$isTip == TRUE]
      
          react_list[["tip_vec"]][[ react_list[["n_anno"]] ]] <- tip_vector
          
          # check if the tips of the current annotation overlap with tips from previous annotations
          current_tips <-
            react_list[["tip_vec"]][[react_list[["n_anno"]]]]
          previous_tips <-
            react_list[["tip_vec"]][-react_list[["n_anno"]]]
          n_overlap <-
            sapply(previous_tips, function(x)
              any(current_tips %in% x))
          n_overlap <- sum(unlist(n_overlap))
          
          # set the clade label offset based on how many sets of previous tips it overlaps
          plotted_tree <- ape::as.phylo(react_list[["tree_plot"]])
          ou <- ceiling(log10(max(ape::node.depth.edgelength(plotted_tree))))
          
          if (react_list[["show_tip_labels"]]) {
            nudge <- max(sapply(plotted_tree$tip.label, nchar)) * 2.25
          } else {
            nudge <- 0
          }
          
          label_offset <- nudge + ou + ou * n_overlap + input[["anno_nudge"]]
          
          make_layer <- function(tree, tips, label, color, offset) {
            ggtree::geom_cladelabel(
              node = ape::getMRCA(ape::as.phylo(tree), tips),
              label = label,
              color = color,
              angle = 0,
              offset = offset
            )
          }
          
          anno_phy <- react_list[["tree_plot"]] +
            make_layer(
              tree = react_list[["tree_plot"]],
              tips =  current_tips,
              color = "grey25",
              label = input[["anno_name"]],
              offset = label_offset
            )
          at <- annotations_table()
          at <- rbind(at, c(input[["anno_name"]], label_offset))
          names(at) <- c("Name", "Nudge")
          annotations_table(at)
          react_list[["tree_plot"]] <- anno_phy
        }
      })
      
      output[["anno_table"]] <- renderTable(
        annotations_table()
      )
      
      return(react_list)
      
    }
  )
}

#' @rdname annotate_server
#' @param id namespace id
#' @export

annotate_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::checkboxInput(
      inputId = "expand_anno",
      label = "Clade annotations",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.expand_anno == true",
      shiny::wellPanel(
        actionButton(
          inputId = ns("add_anno"),
          label = "Add annotation",
          icon = icon("plus")
        ),
        div(style="height: 5px;"),
        actionButton(
          inputId = ns("rm_anno"),
          label = "Remove annotation(s)",
          icon = icon("trash")
        ),
        div(style="height: 5px;"),
        textInput(
          inputId = ns("anno_name"),
          label = "Label",
          value = "Clade",
          width = "100%"
        ),
        div(style="height: 5px;"),
        numericInput(
          inputId = ns("anno_nudge"),
          label = "Horizontal nudge",
          value = 0,
          width = "100%"
        ),
        tags$hr(),
        tableOutput(ns("anno_table"))
      )
    )
  )
}