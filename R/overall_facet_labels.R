# overall_facet_labels
####################################################################
#' add overall facet labels to a ggplot
#'
#' @param p base plot that overall facet labels should be added to
#' @param labelR the name of the overall variable for the facets on the right side of the plot
#' @param labelT the name of the overall variable for the facets on the top side of the plot
#' @param where either "top", "right", or "both" - depends on directions of facets
#' @param input if "ggplot" a conversion to grob is done, otherwise it is assumed that p is a grob already
#' @export
overall_facet_labels <- function(p, labelR = "", labelT = "", where = "top", input = "ggplot")
{
  # removing 3 strip borders
  #https://stackoverflow.com/questions/54471816/remove-three-sides-of-border-around-ggplot-facet-strip-label

  # strip overall titles
  # https://stackoverflow.com/questions/36941197/overall-label-for-facets


  if(input == "ggplot"){
    # Get the ggplot grob
    z <- ggplot2::ggplotGrob(p)

  } else{
    z <- p
  }


  if(where == "top")
  {

    posT <- subset(z$layout, grepl("strip-t", name), select = t:r)
    # and a new row on top of current top strips
    height <- z$heights[min(posT$t)]  # height of current top strips
    z <- gtable::gtable_add_rows(z, height, min(posT$t)-1)

    # get the font information from the current x axis label
    current_fontsize <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$fontsize
    current_fontcol <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$col
    current_font <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$fontfamily

    #current_lwd <- z$grobs[[grep("axis-b-1",z$layout$name)]]$children[[2]]$grob[[1]]$gp$lwd
    current_lwd <- 2
    current_linecol <- z$grobs[[grep("axis-b-1",z$layout$name)[[1]]]]$children[[2]]$grob[[1]]$gp$col



    stripT <- grid::gTree(name = "Strip_top", children = grid::gList(
      grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)),
      grid::textGrob(labelT, gp = grid::gpar(fontsize = current_fontsize, col = current_fontcol, fontfamily = current_font))))
    # Position the grobs in the gtable
    z <- gtable::gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")


    lg <- grid::linesGrob(x=grid::unit(c(1,0),"npc"), y=grid::unit(c(0,0),"npc"),
                    gp=grid::gpar(col=current_linecol, lwd=current_lwd))

    for (k in grep("strip-top",z$layout$name)) {
      #z$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
      # z$grobs[[k]]$grobs[[1]] <- lg
      z$grobs[[k]]$children[[1]] <- lg
    }



  }else if(where == "right"){

    # Get the positions of the strips in the gtable: t = top, l = left, ...
    posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
    # Add a new column to the right of current right strips,
    width <- z$widths[max(posR$r)]    # width of current right strips
    z <- gtable::gtable_add_cols(z, width, max(posR$r))


    # get the font information from the current x axis label
    current_fontsize <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$fontsize
    current_fontcol <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$col
    current_font <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$fontfamily

    #current_lwd <- z$grobs[[grep("axis-b-1",z$layout$name)]]$children[[2]]$grob[[1]]$gp$lwd
    current_lwd <- 2
    current_linecol <- z$grobs[[grep("axis-b-1",z$layout$name)[[1]]]]$children[[2]]$grob[[1]]$gp$col


    # Construct the new strip grobs
    stripR <- grid::gTree(name = "Strip_right", children = grid::gList(
      grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)),
      grid::textGrob(labelR, rot = -90, gp = grid::gpar(fontsize = current_fontsize, col = current_fontcol, fontfamily = current_font))))
    # Position the grobs in the gtable
    #z <- gtable_add_grob(z, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
    z <- gtable::gtable_add_grob(z, stripR, t = min(posR$t), l = max(posR$r) + 1, b = max(posR$b), name = "strip-right")


    # add an overall line for below the overall strip labels
    lg <- grid::linesGrob(x=grid::unit(c(0,0),"npc"), y=grid::unit(c(1,0),"npc"),
                    gp=grid::gpar(col=current_linecol, lwd=current_lwd))

    for (k in grep("strip-right",z$layout$name)) {
      #z$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
      # z$grobs[[k]]$grobs[[1]] <- lg
      z$grobs[[k]]$children[[1]] <- lg
    }

  } else if(where == "both"){

    posT <- subset(z$layout, grepl("strip-t", name), select = t:r)
    # and a new row on top of current top strips
    height <- z$heights[min(posT$t)]  # height of current top strips
    z <- gtable::gtable_add_rows(z, height, min(posT$t)-1)

    # Get the positions of the strips in the gtable: t = top, l = left, ...
    posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
    # Add a new column to the right of current right strips,
    width <- z$widths[max(posR$r)]    # width of current right strips
    z <- gtable::gtable_add_cols(z, width, max(posR$r))

    # get the font information from the current x axis label
    current_fontsize <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$fontsize
    current_fontcol <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$col
    current_font <- z$grobs[[grep("xlab-b",z$layout$name)]]$children[[1]]$gp$fontfamily

    #current_lwd <- z$grobs[[grep("axis-b-1",z$layout$name)]]$children[[2]]$grob[[1]]$gp$lwd
    current_lwd <- 2
    current_linecol <- z$grobs[[grep("axis-b-1",z$layout$name)[[1]]]]$children[[2]]$grob[[1]]$gp$col

    # Construct the new strip grobs
    stripT <- grid::gTree(name = "Strip_top", children = grid::gList(
      grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)),
      grid::textGrob(labelT, gp = grid::gpar(fontsize = current_fontsize, col = current_fontcol, fontfamily = current_font))))
    # Position the grobs in the gtable
    z <- gtable::gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

    stripR <- grid::gTree(name = "Strip_right", children = grid::gList(
      grid::rectGrob(gp = grid::gpar(col = NA, fill = NA)),
      grid::textGrob(labelR, rot = -90, gp = grid::gpar(fontsize = current_fontsize, col = current_fontcol, fontfamily = current_font))))
    # Position the grobs in the gtable
    #z <- gtable::gtable_add_grob(z, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
    z <- gtable::gtable_add_grob(z, stripR, t = min(posR$t), l = max(posR$r) + 1, b = max(posR$b), name = "strip-right")


    lg <- grid::linesGrob(x=grid::unit(c(1,0),"npc"), y=grid::unit(c(0,0),"npc"),
                    gp=grid::gpar(col=current_linecol, lwd=current_lwd))

    for (k in grep("strip-top",z$layout$name)) {
      #z$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
      # z$grobs[[k]]$grobs[[1]] <- lg
      z$grobs[[k]]$children[[1]] <- lg
    }

    # add an overall line for below the overall strip labels
    lg <- grid::linesGrob(x=grid::unit(c(0,0),"npc"), y=grid::unit(c(1,0),"npc"),
                    gp=grid::gpar(col=current_linecol, lwd=current_lwd))

    for (k in grep("strip-right",z$layout$name)) {
      #z$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
      # z$grobs[[k]]$grobs[[1]] <- lg
      z$grobs[[k]]$children[[1]] <- lg
    }


  }


  ggplotify::as.ggplot(z)

}
