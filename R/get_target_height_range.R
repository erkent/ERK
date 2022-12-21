# get_target_height_range
####################################################################
#' get the min and max target z coordinates scanned given the scanner distance, height, and scan pattern
#'
#' @param distance horizontal distance between scanner and target (m)
#' @param scanner_height of the scanner (m)
#' @param theta_min the minimum zenith angle (deg.) at which the scanner sends out a beam (defaults to a value of 30 for the VZ-1000 scanner)
#' @param theta_max the maximum zenith angle (deg.) at which the scanner sends out a beam (defaults to a value of 130 for the VZ-1000 scanner)
#' @export
get_target_height_range <- function(distance, scanner_height, theta_min = 30, theta_max = 130)
{

  h1 = scanner_height + distance/tan(theta_min*(pi/180))

  h2 = scanner_height + distance/tan(theta_max*(pi/180))

  cbind(h1, h2)

}
