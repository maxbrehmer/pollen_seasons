data <- read_csv("data/pollen_counts.csv")

df <- data %>% arrange(station, lat_name, date) %>% mutate(factor_count = count*factor)

print(unique(df$lat_name))