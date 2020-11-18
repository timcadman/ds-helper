> prerequisites:
> - install.packages("devtools")
> - install.packages("styler")
> - install.packages("pkgdown")

#### In general:

* [ ] Did you test the code against `https://armadillo.test.molgenis.org`?
* [ ] Have you ran `styler:::style_active_pkg()` to style the code-files
* [ ] Have you ran `devtools::check()` to be sure documentation is rerendered and code is quality checked?

#### Vignettes:

1. [ ] Did you create an .orig file and add it to the pre-render.R file?
2. [ ] Did you run the the pre-render.R file to regenerate the vignettes
3. [ ] Did you run `pkgdown::build_site()` to preview the documentation
