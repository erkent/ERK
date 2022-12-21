# get_beam_diameter_at_distance
####################################################################
#' get the diameter of a LiDAR beam at distance
#'
#' @param distance distance between scanner and target (m)
#' @param beam_diameter_at_exit the diameter of the LiDAR beam as it exits the scanner (mm)
#' @param beam_divergence the divergence of the beam (mrad)
#' @note output is the beam diameter at the target in mm
#' @note default values of beam_diameter_at_exit and beam_divergence are for the VZ-1000 scanner
#' @export
get_beam_diameter_at_distance <- function(distance, beam_diameter_at_exit = 7, beam_divergence = 0.3)
{

  distance*tan(beam_divergence/2/1000)*1000 + beam_diameter_at_exit

}
