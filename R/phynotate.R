#' Phynotate: Shiny modules for plotting, customization, and export of phylogeny
#' 
#' @name phynotate
#' @docType package
#' @keywords package
#'
#' @import shiny
#'
#' @importFrom stats median quantile sd
#' @importFrom stringr str_replace str_extract str_detect str_remove regex
#' @importFrom ggtree ggtree geom_tiplab geom_tiplab2
#' @importFrom ggplot2 scale_x_continuous aes expansion geom_text
#' @importFrom treeio as.treedata
#' @importFrom viridis viridis
#' @importFrom ape branching.times
#' @importFrom rlang .data
#' 
NULL