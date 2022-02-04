library(dplyr)
library(stringr)
library(readxl)




# entries <- read_entries('entry_lists/Attendees - 2022 PaddleNSW Marathon Series - Race 1 - Oura to Wagga (Downstream, all the way !!) (1).csv')
# 
# 
# juniors <- df %>%
#   filter(str_detect(division_entered, "Junior"))
# 
# adults <- entries %>%
#   filter(!str_detect(division_entered, "Junior")) %>%
#   rowwise() %>%
#   mutate(
#     ranking = ifelse(str_detect(division_entered, "ranking"), TRUE, FALSE),
#     division_entered = as.numeric(str_match(division_entered, "[0-9]*")[,1])
#   ) %>% 
#   ungroup()
# 
# adult_singles <- adults %>% 
#   filter(partner == "")
#   
# single_divs <- read.csv('current_paddler_divs/singles.csv') %>%
#   tibble() %>% 
#   select(first_name, last_name = surname, div)
# 
# adult_singles %>% 
#   left_join(single_divs, by = c("first_name", "last_name"))
# 
# # Run checked
# # Note of changes made
# checked %>%
#   filter(!str_detect(division_entered, "Junior")) %>%
#   filter(!(actual_div > division_entered)) %>% 
#   filter(actual_div != division_entered) %>% 
#   filter(isFALSE(ranking)) %>% View()


#### Functions ####
extract_divisions <- function(entry_category) {
  if (str_detect(entry_category, "Junior")) {
    return(entry_category)
  } else if (str_detect(entry_category, "Ranking")) {
    div <- paste0(str_match(entry_category, "Division ([0-9]*)")[,2], "ranking")
  } else {
    div <- str_match(entry_category, "Division ([0-9]*)")[,2]
  }
  return(div)
}

read_entries <- function(path) {
  read.csv(path) %>%
    tibble() %>%
    janitor::clean_names() %>%
    select(
      first_name, last_name, dob, age, gender, booking_state,
      club = contains("club_select_club_you_are_paddling_for"),
      partner = contains("club_paddle_partner_s_name_if_paddling_a_double"),
      boat_type = contains("club_boat_type"),
      entry_category = contains("division_are_you_entering"),
      email_address,
      age,
      gender
    ) %>%
    filter(booking_state == "Booked") %>%
    rowwise() %>%
    mutate(
      division_entered = extract_divisions(entry_category),
      first_name = str_to_title(str_trim(first_name)),
      last_name = str_to_title(str_trim(last_name))
    ) %>% 
    ungroup()
}

#### Create entry list from clean entries
create_ws_entry_list <- function(paddlers) {
  # Columns required: First name	Last name	Age	Gender	Email	Distance	Team name
  paddlers %>% 
    select(first_name, last_name, age, gender, email_address, entry_category, club, boat_type) %>% 
    arrange(entry_category) %>% 
    rename(distance = entry_category, team_name = club, email = email_address, info_1 = boat_type) %>% 
    janitor::clean_names("title")
}

#### Add bib numbers to entry list #### 
add_bibs <- function(entries) {
  entries %>%
    rowwise() %>% 
    mutate(div = extract_divisions(Distance),
           div = as.numeric(str_remove(div, "ranking"))) %>% 
    group_by(div) %>%
    mutate(rank = rank(`First Name`, ties = "first"),
           bib = (100 * div) + rank - 1) %>% 
    ungroup() %>% 
    select(Bib = bib, `First Name`, `Last Name`, `Age`, `Gender`, `Email`, Distance, `Team Name`, `Info 1`) %>% 
    arrange(Distance, Bib)
}


#### Convert division to a ranking div ###
div_to_ranking <- function(div) {
  div_nm <- str_extract(div, "[a-zA-z 0-9]*(?= - )")
  dist <- str_extract(div, "(?<= - )[a-zA-z0-9]*")
  if (str_detect(div_nm, "8")) {
    return(div)
  }
  paste0(div_nm, " - ", dist, " Ranking")
}
