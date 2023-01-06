library(devtools)
library(usethis)

# create_package("../ERK/")

# use_git()

# use_r("get_beam_diameter_at_distance")

load_all()

# use_gpl3_license()

document()

# use_package("data.table")
# use_package("ggplot2")

# use_readme_rmd()

# devtools::build_readme()


# usethis::use_vignette("LiDAR_functions")
# devtools::build_rmd("./vignettes/LiDAR_functions.Rmd")


# use_r("get_target_height_range")

# use_r("overall_facet_labels")
# usethis::use_vignette("plotting_functions")

devtools::build_rmd(files = "./vignettes/plotting_functions.Rmd")


# use_package("grid")
# use_package("ggplotify")
# use_package("gtable")
# use_package("showtext")
use_package("lemon")



use_r("theme_ERK")



getwd()
check()
build()
