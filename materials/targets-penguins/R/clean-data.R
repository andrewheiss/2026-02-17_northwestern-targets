clean_penguins <- function(x) {
  penguins_raw <- read_csv(x, col_types = cols())
  
  penguins_clean <- penguins |> 
    drop_na(sex)
  
  return(penguins_clean)
}
