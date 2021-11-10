library(knitr)
# please note that rendered images are rendered in the project-root
# you need to copy those to the vignettes directory
# you can not use 'fig.path' because the path is used in the <img> tag which
# then refers to the relative path you type in
knitr::knit("vignettes/ds-helper-data-manipulation.Rmd.orig",
            output = "vignettes/ds-helper-data-manipulation.Rmd"
)
knitr::knit("vignettes/Troubleshooting.Rmd.orig",
            output = "vignettes/Troubleshooting.Rmd"
)