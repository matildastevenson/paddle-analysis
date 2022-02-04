library(purrr)

points <- read.csv('../paddle_nsw_races/series_standings_2021.csv',
                   fileEncoding = "UTF-8-BOM") %>%
  tibble() %>%
  filter(!str_detect(Name, "Fake")) %>%
  janitor::clean_names() %>%
  rowwise() %>%
  mutate(across(starts_with("race"), ~ as.numeric(str_remove_all(str_replace(.x, "-", NA_character_), "[()]"))))

points$race_count <- rowSums(!is.na(select(points, starts_with("race_"))))

points %>% pivot_longer(cols = race_1:race_12, names_to = "race", values_to = "points", names_prefix = "race_") %>%
  filter(!is.na(points)) %>%
  group_by(name) %>%
  arrange(division, name, points) %>%
  slice_max(points, n = 5) %>%
  group_by(name, division) %>%
  summarise(total = sum(points)) %>%
  arrange(desc(division, points))
