library(viridis)
library(RColorBrewer)

pn <-
  c("viridis",
    "magma",
    "inferno",
    "plasma",
    "cividis",
    "BrBG",
    "PiYG",
    "PRGn",
    "PuOr",
    "RdBu",
    "RdYlBu"
  )

get_color_gradient <- function(palette_name) {
  if (palette_name %in% c("viridis", "magma", "inferno", "plasma", "cividis")) {
    X <- viridis::viridis(n = 9, option = palette_name)
  }
  
  if (palette_name %in% c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdYlBu")) {
    X <- RColorBrewer::brewer.pal(n = 9, name = palette_name)
  }
  
  X
}

palettes <- lapply(pn, get_color_gradient)
names(palettes) <- pn
usethis::use_data(palettes, overwrite = TRUE)
