library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "sf") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# The pipeline
list(
  tar_target(state_details, make_state_details()),
  tar_target(county_results_raw, get_election_data()),
  tar_target(
    election_data_clean,
    clean_election_data(county_results_raw, state_details)
  ),
  tar_target(
    shelby_counties,
    get_clean_shelby_county(
      "https://cdn.zevross.com/civilrights/pollmap/v3/data/cnty_data_section5.topojson"
    )
  ),

  tar_target(county_info, tigris::counties(cb = TRUE, resolution = "5m")),
  tar_target(state_info, tigris::states(cb = TRUE, resolution = "5m")),

  tar_target(shelby_map, make_shelby_map(county_info, shelby_counties)),
  tar_target(states_map, make_state_map(state_info, shelby_map)),

  tar_target(shelby_map_png, make_png_map(states_map, shelby_map), format = "file"),

  tar_quarto(answers, "answers.qmd", quiet = FALSE)
)
