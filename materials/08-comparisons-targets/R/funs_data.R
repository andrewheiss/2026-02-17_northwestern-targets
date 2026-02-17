make_state_details <- function() {
  library(tigris)

  regions_info <- regions() |>
    sf::st_drop_geometry() |>
    select(REGION_ID = GEOID, REGION = NAME)

  divisions_info <- divisions(protocol = "ftp") |>
    sf::st_drop_geometry() |>
    select(DIVISION_ID = GEOID, DIVISION = NAME)

  states_info <- states(cb = FALSE) |>
    sf::st_drop_geometry() |>
    select(REGION_ID = REGION, DIVISION_ID = DIVISION, STATEFP, STUSPS, NAME)

  state_details <- states_info |>
    left_join(regions_info, by = join_by(REGION_ID)) |>
    left_join(divisions_info, by = join_by(DIVISION_ID)) |>
    select(
      state = NAME,
      state_abb = STUSPS,
      # state_fips = STATEFP,
      region = REGION,
      division = DIVISION
    )
  
  return(state_details)
}

get_election_data <- function() {
  library(dataverse)

  df <- get_dataframe_by_name(
      filename = "countypres_2000-2024.tab",
      dataset = "doi:10.7910/DVN/VOQCHQ",
      server = "dataverse.harvard.edu",
      version = "19.0"
    )
  
  return(df)
}

clean_election_data <- function(county_results_raw, state_details) {
  # These election results don't include an overall total and instead separate
  # everything out by election day, absentee, early voting, etc., so we have
  # to handle them differently
  # fmt: skip
  weird_elections <- tribble(
  ~state_po, ~year,
  "AZ", 2020,
  "AR", 2020,
  "GA", 2020,
  "IA", 2020,
  "KY", 2020,
  "MD", 2020,
  "NM", 2024,
  "NC", 2020,
  "NC", 2024,
  "OK", 2020,
  "SC", 2020,
  "VA", 2020
)

  county_results_weird <- county_results_raw |>
    # Only look at the weird untotaled elections
    semi_join(weird_elections, by = join_by(state_po, year)) |>
    # fmt: skip
    group_by(
    year, state, state_po, county_name, county_fips,
    office, candidate, party, version
  ) |>
    # Calculate the total votes per candidate/party
    summarize(candidatevotes = sum(candidatevotes)) |>
    ungroup() |>
    # Calculate the correct denominator
    mutate(
      totalvotes = sum(candidatevotes),
      .by = c(year, state_po, county_fips)
    ) |>
    mutate(mode = "TOTAL")

  county_results_normal <- county_results_raw |>
    # Don't work with the weird untotaled elections
    anti_join(weird_elections, by = join_by(state_po, year)) |>
    filter(
      # Some states include over/undervotes; ignore them
      !(candidate %in% c("OVERVOTES", "UNDERVOTES")),
      # Texas has "TOTAL VOTES CAST" as a candidate; ignore it
      candidate != "TOTAL VOTES CAST",
      # Texas also has a weird "OTHER" candidate with inflated counts when the
      # mode is NA; ignore it
      !(state_po == "TX" & candidate == "OTHER" & is.na(mode))
    ) |>
    mutate(mode = replace_na(mode, "TOTAL")) |>
    filter(mode %in% c("TOTAL", "TOTAL VOTES")) |>
    # Calculate the total vote denominator manually
    mutate(
      totalvotes = sum(candidatevotes),
      .by = c(year, state_po, county_fips)
    )

  # Combine both the normal and weird data
  county_results <- bind_rows(
    county_results_normal,
    county_results_weird
  ) |>
    arrange(year, county_fips) |>
    # Get rid of the MIT data state column because it's ALL CAPS
    select(-state) |>
    # Join in the census regions and state names
    left_join(state_details, by = join_by(state_po == state_abb)) |>
    # Rearrange some columns - move the state and region stuff to the front
    select(year, state_po, state, region, division, everything())

  return(county_results)
}

get_clean_shelby_county <- function(url) {
  library(jsonlite)

  tmp_file <- tempfile()
  download.file(url, tmp_file)

  shelby_raw_json <- readLines(tmp_file, warn = FALSE) |>
    paste(collapse = "\n") |>
    # The JSON is assigned to a Javascript variable, so we extract it first
    str_extract("\\{.*\\}") |>
    fromJSON()

  shelby_counties <- as_tibble(
    shelby_raw_json$objects$foo$geometries$properties
  ) |>
    pivot_longer(
      cols = c(benchmark_cnt, postshelby_cnt),
      names_to = "period",
      values_to = "count",
      names_pattern = "(.*)_cnt"
    ) |>
    mutate(
      year = if_else(period == "benchmark", benchmark_yr, postshelby_yr),
      period = if_else(period == "benchmark", "Before", "After")
    ) |>
    select(
      geoid,
      county = name,
      state_po = stabbr,
      year,
      polling_places = count,
      period,
      population = pop_tot,
      prop_high_poverty = povls2x,
      prop_hispanic_latino = hl,
      prop_2more = nhl_2more,
      prop_aian = nhl_aian,
      prop_asian = nhl_asian,
      prop_black = nhl_blk,
      prop_nhpi = nhl_nhpi,
      prop_other = nhl_oth,
      prop_white = nhl_wht
    )

  return(shelby_counties)
}

make_shelby_map <- function(county_info, shelby_counties) {
  library(tigris)

  shelby_map <- county_info |>
    filter(STATEFP < 60) |>
    mutate(preclearance = GEOID %in% shelby_counties$geoid) |>
    mutate(
      preclearance = ifelse(STUSPS %in% c("TX", "AK"), TRUE, preclearance)
    ) |>
    shift_geometry() |>
    filter(preclearance == TRUE)

  return(shelby_map)
}

make_state_map <- function(state_info, shelby_map) {
  library(tigris)

  states_map <- state_info |>
    filter(STATEFP < 60) |>
    shift_geometry() |>
    mutate(is_shelby = STATEFP %in% shelby_map$STATEFP)

  return(states_map)
}

make_png_map <- function(states_map, shelby_map) {
  map_preclearance <- ggplot() +
    geom_sf(data = states_map, aes(fill = is_shelby)) +
    geom_sf(
      data = shelby_map,
      fill = "#ec008b",
      linewidth = 0.05,
      color = "white"
    ) +
    geom_sf(data = states_map, fill = NA, color = "black") +
    scale_fill_manual(values = c("white", "grey85"), guide = "none") +
    coord_sf(crs = st_crs("ESRI:102003")) +
    theme_void()

  ggsave(
    "images/map_preclearance.png",
    map_preclearance,
    width = 9,
    height = 6,
    device = ragg::agg_png,
    res = 600,
    bg = "white"
  )

  return("images/map_preclearance.png")
}
