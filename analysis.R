pic <- df %>% filter(year == 1980) %>% ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_wrap(~ lat_name)

print(pic)