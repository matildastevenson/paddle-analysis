# This was used to create the master list of paddler divisions from Don's
# spreadsheet. The second section updates the spreadsheet after a race
library(dplyr)
library(stringr)

# read original spreadsheet
p_div <- readxl::read_xlsx('paddler_divisions_useful.xlsx') %>% 
  janitor::clean_names() %>% 
  rename(div = next_div) %>% 
  mutate(first_slow = NA,
         first_fast = NA,
         double = ifelse(str_count(first_name, pattern = boundary("word")) > 1 &
                           str_count(surname, pattern = boundary("word")) > 1,
         TRUE, FALSE))

p_div_single <- p_div %>% 
  filter(double == FALSE) %>% 
  mutate(name = str_to_upper(paste(first_name, surname)))

write.csv(p_div_single, file = 'current_paddler_divs/singles.csv',
          row.names = FALSE)

p_div_double <- p_div %>% 
  filter(double == TRUE)

write.csv(p_div_double, file = 'current_paddler_divs/doubles.csv',
          row.names = FALSE)

# TODO: match doubles in results and division file



paddler_alias <- function(name) {
  switch (name,
          "MICHAEL LEVERETT" = "MICK LEVERETT",
          name
  )
}

read_results <- function(path) {
  path <- 'results/narrabeen_10_22.xls'
  readxl::read_xls(path, skip = 1) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(place), place != "Place") %>% 
    select(place, name, club = team_name, distance, time, difference, team_points, percent_median) %>% 
    mutate(div = str_extract(distance, "(?<=Division )[0-9]*")) %>% 
    mutate(ranking = ifelse(str_detect(distance, "Ranking"), TRUE, FALSE)) %>% 
    filter(!str_detect(distance, "Junior")) %>% 
    filter(time != 'DNS') %>% 
    group_by(name, club, distance, time) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(name = str_to_upper(name),
           name = paddler_alias(name)) %>% 
    ungroup()
}



# Take webscorer results and update paddler divisions
results <- read_results('results/narrabeen_10_22.xls')

single_results <- results %>% 
  filter(str_count(name, pattern = boundary("word")) < 4)

double_results <- results %>%
  filter(str_count(name, pattern = boundary("word")) > 3)

single_div_test <- single_results %>% 
  rename(race_div = div) %>% 
  left_join(p_div_single, by = "name")
