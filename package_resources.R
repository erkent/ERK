library(devtools)

create_package("../ERK/")

use_git()

# use_r("get_beam_diameter_at_distance")

load_all()

use_gpl3_license()

document()

use_package("data.table")
use_package("ggplot2")

use_readme_rmd()

devtools::build_readme()


library(usethis)
# usethis::use_vignette("LiDAR_functions")
devtools::build_rmd("./vignettes/LiDAR_functions.Rmd")


# use_r("get_target_height_range")

