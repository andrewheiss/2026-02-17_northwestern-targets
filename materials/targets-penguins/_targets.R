library(targets)
library(tarchetypes)

suppressPackageStartupMessages(library(brms))
options(
  mc.cores = 4
)

# Set target options:
tar_option_set(
  packages = c("tidyverse") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  tar_target(penguins_file, "data/penguins.csv", format = "file"),
  tar_target(penguins, clean_penguins(penguins_file)),
  
  tar_target(model1_freq, make_model1_freq(penguins)),
  tar_target(model2_freq, make_model2_freq(penguins)),
  tar_target(model1_bayes, make_model1_bayes(penguins)),
  tar_target(model2_bayes, make_model2_bayes(penguins)),
  
  tar_quarto(analysis, "analysis.qmd", quiet = FALSE)
)
