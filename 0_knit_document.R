
rmarkdown::render(
  input = "./manuscript.rmd",
  output_format = "papaja::apa6_word",
  output_dir = "./output",
  output_file = paste0("apa7_document"),
  intermediates_dir = "markdown",
  knit_root_dir = file.path(rprojroot::find_rstudio_root_file()),
  clean = TRUE
  # params = params # can set author and date in a vector here
)
