data <- read_csv("data/pollen_counts.csv")

latin_names <- c("Alnus", "Betula", "Corylus", "Poaceae", "Quercus", "Salix", "Ulmus")

df <- data %>% arrange(station, lat_name, date) %>% mutate(year = as.numeric(year(date))) %>%
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
  

df <- df %>% uncount(count) %>% mutate(greg_day = as.numeric(yday(date))) %>% mutate(md_date = as_date(paste("00", format(date, format = "%m-%d"), sep = "-")) ) %>% 
  dplyr::select(c("station", "lat_name", "year", "greg_day", "md_date", "date", "latitude")) %>%
  drop_na()

genus <- c("Alnus", "Betula", "Poaceae", "Quercus", "Salix", "Ulmus", 
           "Alnus", "Betula", "Poaceae", "Quercus", "Salix", "Ulmus", 
           "Alnus", "Betula", "Poaceae", "Quercus", "Salix", 
           "Alnus", "Betula", "Poaceae", "Quercus", "Salix", "Ulmus", 
           "Alnus", "Betula", "Poaceae", "Quercus", "Salix", "Ulmus", 
           "Alnus", "Betula", "Corylus", "Poaceae", "Quercus", "Salix", "Ulmus", 
           "Alnus", "Betula", "Poaceae", "Salix", 
           "Alnus", "Betula", "Poaceae", "Quercus", "Salix", "Ulmus")

place <- c("Esklistuna", "Esklistuna", "Esklistuna", "Esklistuna", "Esklistuna", "Esklistuna",
           "Göteborg", "Göteborg", "Göteborg", "Göteborg", "Göteborg", "Göteborg", 
           "Jönköping", "Jönköping", "Jönköping", "Jönköping", "Jönköping", 
           "Malmö", "Malmö", "Malmö", "Malmö", "Malmö", "Malmö", 
           "Norrköping", "Norrköping", "Norrköping", "Norrköping", "Norrköping", "Norrköping", 
           "Stockholm", "Stockholm", "Stockholm", "Stockholm", "Stockholm", "Stockholm", "Stockholm", 
           "Umeå", "Umeå", "Umeå", "Umeå",
           "Västervik", "Västervik", "Västervik", "Västervik", "Västervik", "Västervik")

n <- c(10, 91, 10, 12, 7, 2, 
       7, 99, 15, 11, 4, 3, 
       9, 48, 7, 6, 3, 
       7, 41, 18, 9, 4, 8, 
       5, 58, 11, 6, 3, 2, 
       12, 75, 2, 11, 17, 4, 7, 
       4, 43, 5, 2, 
       7, 47, 8, 9, 2, 2)

# Apply the function with varying values of n
n_values <- c(10, 91, 1, 10, 12, 7, 2, 
              7, 99, 1, 15, 11, 4, 3, 
              9, 48, 1, 7, 6, 3, 1,
              7, 41, 1, 18, 9, 4, 8, 
              5, 58, 1, 11, 6, 3, 2, 
              12, 75, 2, 11, 17, 4, 7, 
              4, 43, 1, 5, 1, 2, 1, 
              7, 47, 1, 8, 9, 2, 2)

for (i in 1:length(genus)) {
  df <- df %>%
    filter(lat_name == genus[i], station == place[i]) %>%
    slice(which(row_number() %% n[i] == 1)) %>%
    bind_rows(df %>% filter(!(lat_name == genus[i] & station == place[i])))
}

df <- df %>% arrange(station, lat_name, date)





