#' qsm_tubes_to_Helios_xml
#'
#' @param cylinder_file cylinder text output file from TreeQSM (need to set inputs.savetxt = 1 in create_input.m of TreeQSM)
#' @param output_file name of the xml file to be written
#' @param subdiv integer number of subdivisions to be used in tube object construction
#' @param data_fields vector of column names from input_file that should be written as cylinder object data in the xml file
#' @export
qsm_cylinders_to_Helios_tubes_xml <- function(cylinder_file, output_file, subdiv = 5, data_fields){

  if(missing(data_fields))
  {
    data_fields <- c("segment_length", "segment_radius", "segment_ID")
  }

  # force subdiv to be an integer
  subdiv <- as.integer(subdiv)

  df <- assign_qsm_segments(cylinder_file)

  if(file.exists(output_file))
  {
    file.remove(output_file)
  }

  fileConn <- file(output_file, open="a")

  l1 <- c("<?xml version=\"1.0\"?>",
          "",
          "<helios>")

  writeLines(l1, fileConn)

  unique_segments <- unique(df$segment_ID)
  unique_segments <- sort(unique_segments)

  i = 1
  for(i in 1:length(unique_segments))
  {
    this_seg <-  df[segment_ID == unique_segments[i]]
    this_seg <- this_seg[order(PositionInBranch_ID)]

    # hardwired these for now, could add as arguments later
    transform <- paste(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1, sep = " ")
    color <- paste(0,0.75,0, sep = " ")
    #  here just write a default texture, but don't need to with the next Helios version when bug is fixed
    # texture <- "../../../plugins/visualizer/textures/wood.jpg"

    # create vector of object data types for the specified data fields
    types <- data.table(type = do.call(rbind, lapply(this_seg[, data_fields, with = FALSE], typeof)))
    names(types) <- "type"
    types[type == "integer", ft := "data_uint"]
    types[type == "double", ft := "data_float"]
    types[type == "logical", ft := "data_logical"]
    types[type ==  "character", ft := "data_string"]

    l2 <- c("  <tube>",
            paste0("    <objID>",i,"</objID>"),

            # paste0("    <texture>", texture, "</texture>"),
            paste0("    <transform>", transform, "</transform>"),
            paste0("    <subdivisions>", subdiv, "</subdivisions>"))

    writeLines(l2, fileConn)

    # write the object data
    for(j in 1:length(data_fields))
    {
      writeLines( paste0("    <", types[j,ft], " label=\"", data_fields[j], "\">", this_seg[1,data_fields[j], with=FALSE],"</", types[j,ft], ">"), fileConn)
    }

    # build the node, radius, and color lines
    nz <- vector()
    rz <- vector()
    cz <- vector()
    for(j in 1:nrow(this_seg))
    {
      nz[j] <- paste0("       ", this_seg[j, startX], " ", this_seg[j, startY], " ",this_seg[j,startZ])
      rz[j] <- paste0("       ", this_seg[j, radius])
      cz[j] <-  paste0("       ", color)
    }

    l3 <- c("    <nodes>",
            nz,
            paste0("       ", this_seg[nrow(this_seg), endX], " ", this_seg[nrow(this_seg), endY], " ",this_seg[nrow(this_seg), endZ]),
            "    </nodes>",
            "    <radius>")

    l4 <- c(rz,
            paste0("       ", this_seg[nrow(this_seg), radius]),
            "    </radius>",
            "    <color>",
            cz,
            paste0("       ", color),
            "    </color>")


    writeLines(l3, fileConn)
    writeLines(l4, fileConn)
    writeLines("  </tube>", fileConn)
  }
  writeLines("</helios>", fileConn)
  close(fileConn)
  print("tubes XML file written")
}
