pic2022 <- df %>% filter(year == 2022) %>% ggplot(aes(x = date, y = lat_name)) +
  geom_density_ridges(rel_min_height = 0.005) +
  theme_ridges() +
  facet_wrap(~ station)

pic1990 <- df %>% filter(year == 1990) %>% ggplot(aes(x = date, y = lat_name)) +
  geom_density_ridges(rel_min_height = 0.005) +
  theme_ridges() +
  facet_wrap(~ station)

data_structure <- data.frame("Variable" = c("Station", "Pollen type", "Date", "Factor"), "Type" = c("categorical", "categorical", "continous", "continous"), "Decription" = c(
  "The geographic location of the pollen monitoring station", "", "", ""
))