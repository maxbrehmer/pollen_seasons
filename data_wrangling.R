data <- read_csv("data/pollen_counts.csv")

latin_names <- c("Alnus", "Betula", "Corylus", "Poaceae", "Quercus", "Salix", "Ulmus")

df <- data %>% arrange(station, lat_name, date) %>% mutate(year = year(date)) %>%
  mutate(lat_name = case_when(
    swe_name == "Alnus" ~ "Alnus",
    swe_name == "Al" ~ "Alnus",
    swe_name == "Betula" ~ "Betula",
    swe_name == "Björk" ~ "Betula",
    swe_name == "Corylus" ~ "Corylus",
    swe_name == "Hassel" ~ "Corylus",
    swe_name == "Poaceae" ~ "Poaceae",
    swe_name == "Gräs" ~ "Poaceae",
    swe_name == "Quercus" ~ "Quercus",
    swe_name == "Ek" ~ "Quercus",
    swe_name == "Salix" ~ "Salix",
    swe_name == "Sälg och viden" ~ "Salix",
    swe_name == "Ulmus" ~ "Ulmus",
    swe_name == "Alm" ~ "Ulmus",
    ))

latitudes <- data.frame("station" = c("Umeå", "Eskilstuna", "Stockholm", "Norrköping", "Jönköping", "Västervik", "Göteborg", "Malmö"),
                        "latitude" = c(62.83, 59.37, 59.33, 58.59, 57.78, 57.76, 57.71, 55.60))

df <- full_join(df, latitudes, by = c("station" = "station"))

df <- df %>% uncount(count) %>% mutate(md_date = format(date, format = "%m-%d"))