# theme_ERK
####################################################################
#' custom theme
#' @param facets if TRUE, decreases spacings between facet panels for use with facet_rep_grid from lemon package
#' @note this is a custom theme
#' @export
theme_ERK <- function(facets = FALSE){

  if(facets)
  {
    # this version removes the borders around individual panels and moves the panels closer together for use with facet_rep_grid
    theme_classic() %+replace%
      theme(#axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(-1.5, "lines"),
        panel.spacing.y = unit(0, "lines"))

  }else{

    theme_classic() %+replace%
      theme(#axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank())
  }
}
