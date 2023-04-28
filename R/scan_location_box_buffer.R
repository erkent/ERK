# scan_location_box_buffer
#' get the location of a bounding box that encompasses all passed in scan locations + a buffer distance
#'
#' @param x x coordinates of scan locations
#' @param y coordinates of scan locations
#' @param z coordinates of scan locations
#' @param buffer desired buffer distance
#' @export
scan_location_box_buffer <- function(x,y,z,buffer)
{
  c(min(x) - buffer[1],
    max(x) - buffer[1],
    min(y) - buffer[2],
    max(y) - buffer[2],
    min(z) - buffer[3],
    max(z) - buffer[3])
}


