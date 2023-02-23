data <- read_csv("data/pollen_counts.csv")

df <- data %>% arrange(station, lat_name, date) %>% mutate(year = year(date)) %>% select(-factor)

df <- na.omit(df)

print(unique(df$lat_name))

latitudes <- data.frame("station" = c("Umeå", "Eskilstuna", "Stockholm", "Norrköping", "Jönköping", "Västervik", "Göteborg", "Malmö"),
                        "latitude" = c(62.83, 59.37, 59.33, 58.59, 57.78, 57.76, 57.71, 55.60))

df <- full_join(df, latitudes, by = c("station" = "station"))

df <- df %>% uncount(count) %>% mutate(md_date = format(date, format = "%m-%d"))
