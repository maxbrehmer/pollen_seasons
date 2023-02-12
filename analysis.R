pic2022 <- df %>% filter(year == 2022) %>% ggplot(aes(x = date, y = lat_name)) +
  geom_density_ridges(rel_min_height = 0.005) +
  theme_ridges() +
  facet_wrap(~ station)

pic1990 <- df %>% filter(year == 1990) %>% ggplot(aes(x = date, y = lat_name)) +
  geom_density_ridges(rel_min_height = 0.005) +
  theme_ridges() +
  facet_wrap(~ station)

#ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
#  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#  scale_fill_viridis(name = "Temp. [F]", option = "C") +
#  labs(title = 'Temperatures in Lincoln NE in 2016') +
#  theme_ipsum() +
#  theme(
#    legend.position="none",
#    panel.spacing = unit(0.1, "lines"),
#    strip.text.x = element_text(size = 8)
#  )