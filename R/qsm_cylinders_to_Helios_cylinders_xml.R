#' qsm_cylinders_to_Helios_cylinders_xml
#'
#' @param cylinder_file cylinder text output file from TreeQSM (need to set inputs.savetxt = 1 in create_input.m of TreeQSM)
#' @param output_file name of the xml file to be written
#' @param subdiv integer number of subdivisions to be used in cylinder construction
#' @param data_fields vector of column names from input_file that should be written as cylinder object data in the xml file
#' @param branch_file optional branch text output file from TreeQSM (need to set inputs.savetxt = 1 in create_input.m of TreeQSM)
#' @note if data_fields is not provided, all columns of the TreeQSM output (cylinder_file and branch_file if provided) are written as object data
#' @note if branch_file is provided "branch_" is appended to the beginning of each of that file's column names before joining to the cylinder file. If data_fields is provided, append "branch_" when referencing these column names.
#' @export
qsm_cylinders_to_Helios_cylinders_xml <- function(cylinder_file, output_file, subdiv = 5, data_fields, branch_file){

  # if no data_fields are provided, use all fields not used in constructing the cylinder geometry
  if(missing(data_fields) && missing(branch_file))
  {
    data_fields <- c("parent", "extension",
                     "branch", "branch_order", "position_in_branch",
                     "mad", "SurfCov", "added", "unmodRadius")
  }else if(missing(data_fields)){

    data_fields <- c(c("parent", "extension",
                       "branch", "branch_order", "position_in_branch",
                       "mad", "SurfCov", "added", "unmodRadius"),
                     paste0("branch_",c("parent", "diameter", "volume", "area", "length", "height", "angle", "azimuth", "zenith")))
  }

  # force subdiv to be an integer
  subdiv <- as.integer(subdiv)

  # read in the TreeQSM cylinder output
  df <- fread(cylinder_file, skip = 1, header = FALSE)
  names(df) <- c("radius", "length", "start_x", "start_y", "start_z",
                 "axis_x", "axis_y", "axis_z", "parent", "extension",
                 "branch", "branch_order", "position_in_branch",
                 "mad", "SurfCov", "added", "unmodRadius")

  # calculate the ending node of each cylinder
  df[, end_x := start_x + axis_x*length]
  df[, end_y := start_y + axis_y*length]
  df[, end_z := start_z + axis_z*length]

  # join in the branch file columns if provided
  if(!missing(branch_file))
  {
    br <- fread(branch_file, skip = 1, header = FALSE)
    names(br) <- paste0("branch_",c("order", "parent", "diameter", "volume", "area", "length", "height", "angle", "azimuth", "zenith"))
    br[, branch := 1:nrow(br)]

    df <- merge(df, br, by = c("branch", "branch_order"))
  }


  # hardwired these for now, could add as arguments later
  transform <- paste(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1, sep = " ")
  color <- paste(1,0,0,0, sep = " ")
  #  here just write a default texture, but don't need to with the next Helios version when bug is fixed
  # texture <- "../../../plugins/visualizer/textures/wood.jpg"

  # create vector of object data types for the specified data fields
  types <- data.table(type = do.call(rbind, lapply(df[, data_fields, with = FALSE], typeof)))
  names(types) <- "type"
  types[type == "integer", ft := "data_uint"]
  types[type == "double", ft := "data_float"]
  types[type == "logical", ft := "data_logical"]
  types[type ==  "character", ft := "data_string"]


  if(file.exists(output_file))
  {
    file.remove(output_file)
  }

  fileConn <- file(output_file, open="a")

  l1 <- c("<?xml version=\"1.0\"?>",
          "",
          "<helios>")

  writeLines(l1, fileConn)

  for(i in 1:nrow(df))
  {

    l2 <- c("  <cone>",
            paste0("    <objID>",i,"</objID>"),
            paste0("    <color>",color, "</color>"),
            paste0("    <texture>", texture, "</texture>"),
            paste0("    <transform>", transform, "</transform>"),
            paste0("    <subdivisions>", subdiv, "</subdivisions>"),
            "    <nodes>",
            paste0("       ", df[i, start_x], " ", df[i, start_y], " ",df[i,start_z]),
            paste0("       ", df[i, end_x], " ", df[i, end_y], " ",df[i, end_z]),
            "    </nodes>",
            "    <radius>",
            paste0("       ", df[i, radius]),
            paste0("       ", df[i, radius]),
            "    </radius>")

    writeLines(l2, fileConn)

    # write the object data
    for(j in 1:length(data_fields))
    {
      writeLines( paste0("    <", types[j,ft], " label=\"", data_fields[j], "\">", df[i,data_fields[j], with=FALSE],"</", types[j,ft], ">"), fileConn)
    }

    writeLines("  </cone>", fileConn)
  }
  writeLines("</helios>", fileConn)
  close(fileConn)
  print("XML file written")
}
