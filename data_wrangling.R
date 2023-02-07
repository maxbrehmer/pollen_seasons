data <- read_csv("data/pollen_counts.csv")

df <- data %>% arrange(station, lat_name, date)

print(unique(df$lat_name))