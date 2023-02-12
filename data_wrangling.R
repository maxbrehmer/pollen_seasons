data <- read_csv("data/pollen_counts.csv")

df <- data %>% arrange(station, lat_name, date) %>% mutate(year = year(date))

print(unique(df$lat_name))

df <- df %>% uncount(count) %>% mutate(md_date = format(date, format = "%m-%d"))