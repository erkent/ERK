#' assign_segments
#'
#' @param cylinders either a string specifying a text file or a data.frame with the cylinder text output file from TreeQSM (need to set inputs.savetxt = 1 in create_input.m of TreeQSM)
#' @param output_file name of the xml file to be written
#' @param subdiv integer number of subdivisions to be used in tube construction
#' @param data_fields vector of column names from input_file that should be written as cylinder object data in the xml file
#' @param branch_file optional branch text output file from TreeQSM (need to set inputs.savetxt = 1 in create_input.m of TreeQSM)
#' @note if data_fields is not provided, all columns of the TreeQSM output (cylinder_file and branch_file if provided) are written as object data
#' @note if branch_file is provided "branch_" is appended to the beginning of each of that file's column names before joining to the cylinder file. If data_fields is provided, append "branch_" when referencing these column names.
#' @export
#'
assign_qsm_segments <- function(cylinders)
{

  if(is.character(cylinders))
  {
    # read in the TreeQSM cylinder output
    cylinders <- fread(cylinders, skip = 1, header = FALSE)
    names(cylinders) <- c("radius", "length", "startX", "startY", "startZ",
                          "axisX", "axisY", "axisZ", "parent", "extension",
                          "branch", "branch_order", "position_in_branch",
                          "mad", "SurfCov", "added", "unmodRadius")

  }else if(is.data.frame(cylinders)){

  }

  data <- cylinders



  # Adapted from aRchi R package

  data.table::setnames(data, c("radius", "length", "startX", "startY", "startZ",
                               "axisX", "axisY", "axisZ", "parent_ID", "extension_ID",
                               "branch_ID", "BranchOrder", "PositionInBranch_ID",
                               "mad", "SurfCov", "added", "UnmodRadius"))
  ####################
  #######Step 1#######
  ####################
  # Adapt the format to have cylinder based data instead of circle based data. Similar to AMAPstudio-scan Format.

  data$endX=data$startX+(data$length*data$axisX) #estimates the coordinates of the end of the cylinders. X
  data$endY=data$startY+(data$length*data$axisY) #estimates the coordinates of the end of the cylinders. Y
  data$endZ=data$startZ+(data$length*data$axisZ) #estimates the coordinates of the end of the cylinders. Z
  data$cyl_ID=1:nrow(data)

  # Remove disconnected cylinders (i.e. with parent == 0 that are not part of the branch == 1)
  disconnected_branches <-  data[parent_ID == 0 & branch_ID != 1, branch_ID]
  data <- data[!(branch_ID %in%  disconnected_branches)]

  # Replace coordinates of the start of the children by  the end of the parents. AMAPstudio format.
  f <- function(b) data[which(data$cyl_ID %in% data$parent_ID[b$id])][, c("endX", "endY", "endZ")]
  data[, c("startX", "startY", "startZ") := f(.BY), by=.(id = 1:nrow(data))]

  data$radius_cyl=0 # Radius of the cylinder

  # The radius of the cylinder is the radius of the start circle i.e the parent. Except when there is a ramification, see below
  f2 <- function(b) data[which(data$cyl_ID %in% data$parent_ID[b$id])][, "radius"]
  data[, "radius_cyl" := f2(.BY), by=.(id = 1:nrow(data))]
  data[, .(cyl_ID, parent_ID, radius, radius_cyl)]

  # When a ramification: The radius of the first cylinder of a branch is the radius of the daughter. AMAPstudio format
  data[data$PositionInBranch_ID==1, "radius_cyl"]=data[data$PositionInBranch_ID==1, "radius"]

  ####################
  #######Step 2#######
  ####################

  #######################################################################################################
  #### a. Correct some topological errors/incoherence in terms of topology coming from treeQSM output####
  #######################################################################################################


  # Correcting a treeQSM output error: some cylinder have no extension ID (i.e 0)
  # but they exist as parents ID => incoherence

  # ERK note: Currently treeQSM output is not necessarily incoherent in this way:
  # extension_ID is referring to extensions of the same branch,
  # not other branches which it might be a parent to.
  # if extension_ID was referring to all children of a given cylinder it would need to be a vector
  # Is the "incoherence" that a given cylinder at the end of the branch is assumed not to have any other branches coming from it,
  # and so if that the other branch must actually be an extension of the first branch?
  # this would seem to preclude splits (e.g., scaffolds from a trunk) and spurs at the end of branches

  # the code below tests if any cylinders that have extension_ID == 0 do not have the maximum PositionInBranch_ID value for their branch
  # data[, cyl_in_branch := max(PositionInBranch_ID), .(branch_ID)]
  # test <- df[extension_ID == 0, .(PositionInBranch_ID, cyl_in_branch)]
  # any(test$PositionInBranch_ID != test$cyl_in_branch)

  #commented this out for now...
  # a table with the last cylinder of branches that are not connected to their parents (i.e the parent extension is 0)
  sub_table_cyl_error=data[parent_ID%in%data[extension_ID==0][cyl_ID%in%data$parent_ID]$cyl_ID]

  if(length(unique(data$branch_ID))==1){
    segment_ID=rep(max(data$cyl_ID), nrow(data))
    Node_ID=rep(0, nrow(data))
    data=cbind(data, segment_ID, Node_ID)
    message("This data is only a trunk: no ramification")
    data$volume=(pi*(data$radius_cyl)^2)*data$length
    data$node_ID=0
    data=data[, c("startX", "startY", "startZ", "endX", "endY", "endZ", "cyl_ID", "parent_ID", "extension_ID", "radius_cyl", "length", "volume", "branch_ID", "segment_ID", "node_ID", "BranchOrder")]
    names(data)[c(13, 16)]=c("axis_ID", "branching_order")
    out=list(QSM=data, model=model)
    return(out)
  } # In case of only one branch = a trunk.

  if(nrow(sub_table_cyl_error)!=0){

    # same table but only one child branch is selected when two or more children (the first one)
    sub_table_cyl_error_no_dup=sub_table_cyl_error[duplicated(sub_table_cyl_error$parent_ID)==FALSE]
    # correction of the problem by assigning the good extension ID to the parent
    data[extension_ID==0][cyl_ID%in%data$parent_ID]$extension_ID=sub_table_cyl_error_no_dup$cyl_ID
    # This vector is the branch ID that should not exist as they follow a branch without any ramification. This second problem comes directly from the same treeQSM incoherence detected and corrected above
    branch_ID_to_replace=sub_table_cyl_error_no_dup$branch_ID

    # this is the vector with the good branch ID that should replace the previous one. the loop below make this replacement.
    branch_ID_who_replace=data[cyl_ID%in%sub_table_cyl_error_no_dup$parent_ID]$branch_ID
    for (i in 1:length(branch_ID_to_replace)){


      if(all(is.na(match(unique(data$branch_ID), branch_ID_who_replace[i])))){
        # This is the branch_ID of the parent.
        # if the branch_ID which is supposed to replace does not exist in the branch ID of the whole QSM,
        # it means that it has already been replaced and thus the branch_ID of the parent ID can be used.
        new_branch_ID=data[cyl_ID==data[branch_ID==branch_ID_to_replace[i]]$parent_ID[1]]$branch_ID
        data[branch_ID==branch_ID_to_replace[i]]$branch_ID=rep(new_branch_ID, nrow(data[branch_ID==branch_ID_to_replace[i]]))
        data[branch_ID==new_branch_ID]$PositionInBranch_ID=seq(1, nrow(data[branch_ID==new_branch_ID]))
        next()
      }
      data[branch_ID==branch_ID_to_replace[i]]$branch_ID=rep(branch_ID_who_replace[i], nrow(data[branch_ID==branch_ID_to_replace[i]]))
      data[branch_ID==branch_ID_who_replace[i]]$PositionInBranch_ID=seq(1, nrow(data[branch_ID==branch_ID_who_replace[i]]))

    }
  }

  #################################################
  #### b. Assign a segment_ID to each cylinder ###
  ################################################

  # The nested loop below assigns a segment ID to each cylinder.
  # A segment is a succession of cylinders between two ramification points

  data$segment_ID = 0

  b = 1
  for(b in unique(data$branch_ID)){

    # cylinders on the current branch that are parents to other branches
    tab_segm_lim_b = data[branch_ID == b & cyl_ID %in% data[PositionInBranch_ID == 1, ]$parent_ID, ]

    # if a branch does not have any ramification
    if(nrow(tab_segm_lim_b)==0){
      vec_segment_ID_i = rep(max(data[branch_ID == b, ]$cyl_ID), nrow(data[branch_ID == b, ]))
      next
    }

    # otherwise it does have ramification
    vec_segment_ID_branche_b = NULL
    i = 2
    for (i in 1:(nrow(tab_segm_lim_b)+1)){
      if(i == 1){
        cyl_position_min = min(data[branch_ID == b, ]$PositionInBranch_ID) - 1
        cyl_position_max = tab_segm_lim_b[i, ]$PositionInBranch_ID
        vec_segment_ID_i = rep(tab_segm_lim_b[i, ]$cyl_ID, cyl_position_max - cyl_position_min)

      }else if(i > 1 & i < nrow(tab_segm_lim_b) + 1){
        cyl_position_min = tab_segm_lim_b[i-1, ]$PositionInBranch_ID
        cyl_position_max = tab_segm_lim_b[i, ]$PositionInBranch_ID
        vec_segment_ID_i = rep(tab_segm_lim_b[i, ]$cyl_ID, cyl_position_max - cyl_position_min)

      }else if(i == nrow(tab_segm_lim_b) + 1){
        cyl_position_min = tab_segm_lim_b[i-1, ]$PositionInBranch_ID
        cyl_position_max = max(data[branch_ID == b, ]$PositionInBranch_ID)
        cyl_segm_max=max(data[branch_ID == b, ]$cyl_ID)
        vec_segment_ID_i = rep(cyl_segm_max, cyl_position_max - cyl_position_min)
      }

      if(length(vec_segment_ID_i) == 0){break}

      vec_segment_ID_branche_b = c(vec_segment_ID_branche_b, vec_segment_ID_i)
    }

    data[branch_ID == b, ]$segment_ID = vec_segment_ID_branche_b

  }

  # The nested loop above does not manage the twigs (i.e terminal segments). It is done here.
  segment_ID_twigs = plyr::ddply(data[segment_ID==0, ], ~branch_ID, function(x){cbind(x, segment_ID_2=rep(max(x$cyl_ID), nrow(x)))})
  segment_ID_twigs = segment_ID_twigs[match(data[segment_ID==0, ]$cyl_ID, segment_ID_twigs$cyl_ID), ]
  data[segment_ID==0, ]$segment_ID=segment_ID_twigs$segment_ID_2

  # add segment length and radius
  data[, segment_length := sum(length), .(segment_ID)]
  data[, segment_radius := mean(radius), .(segment_ID)]

  data

}
