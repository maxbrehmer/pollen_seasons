ridges <- df %>% filter(year %in% c(1973, 1974, 1975, 1976, 1977, 2018, 2019, 2020, 2021, 2022), station == "Stockholm") %>%
  mutate(first5 = ifelse(year < 1980, "1973-1977", "2018-2022")) %>%
  ggplot(aes(x = md_date, y = lat_name)) +
  geom_density_ridges(rel_min_height = 0.01, scale = 1.5, aes(fill = first5)) +
  scale_x_date(breaks = seq(as.Date("2000-02-01"), as.Date("2000-09-01"), by = "1 months"), date_labels = "%b", expand = c(0,0), limits = c(as.Date("2000-02-01"), as.Date("2000-09-01")) ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(values = c("1973-1977" = "lightgreen", "2018-2022" = "lightpink")) +
  labs(title = "Average annual distribution of pollen\n in Stockholm (1973-1977) and (2018-2022)", x = "Date", y = "Intensity", fill = "Year group") +
  theme_ridges()


data_structure <- data.frame("Variable" = c("Station", "Species", "Date", "Count", "Factor", "Latitude"), "Type" = c("categorical", "categorical", "continous", "continous", "continous", "continous"), "Decription" = c(
  "Geographic location of the pollen monitoring station.", "Genus of the recorded pollen counts.", "Gregorian calendar date on which the airborne pollen were registered.", 
  "Number of individual pollen counted.", "Reference variable for the size of the microscope used.", "Northern latitudinal cooardinates of said station."
))

stations <- data.frame("Station" = c("Umeå", "Eskilstuna", "Stockholm", "Norrköping", "Jönköping", "Västervik", "Göteborg", "Malmö"), "Latitude" = c(62.83, 59.37, 59.33, 58.59, 57.78, 57.76, 57.71, 55.60), 
                       "Pollen_types" = c("Alnus, Betula, Poaceae, Ulmus (1979), Salix (1981), Corylus (1987), Quercus (1995)", 
                                          "Alnus, Betula, Corylus, Poaceae, Quercus, Salix, Ulmus (1976)", 
                                          "Alnus, Betula, Corylus, Poaceae, Quercus, Ulmus (1973), Salix (1977)", 
                                          "Alnus, Betula, Corylus, Poaceae, Quercus, Salix, Ulmus (1987)", 
                                          "Alnus, Betula, Poaceae, Quercus, Salix, Ulmus (1988), Corylus (1989)", 
                                          "Alnus, Betula, Corylus, Poaceae, Quercus, Salix, Ulmus (1987)",
                                          "Alnus, Betula, Corylus, Poaceae, Quercus, Salix, Ulmus (1979)",
                                          "Alnus, Betula, Corylus, Poaceae, Quercus, Salix, Ulmus (1979)"))

colnames(stations) <- c("Station", "Latitude", "Pollen genus (since ...)")

eskil <- df %>% filter(station == "Eskilstuna") %>% arrange(lat_name, date)
sthlm <- df %>% filter(station == "Stockholm") %>% arrange(lat_name, date)
umea <- df %>% filter(station == "Umeå") %>% arrange(lat_name, date)
jonkp <- df %>% filter(station == "Jönköping") %>% arrange(lat_name, date)
norrkp <- df %>% filter(station == "Norrköping") %>% arrange(lat_name, date)
vastvik <- df %>% filter(station == "Västervik") %>% arrange(lat_name, date)
gbg <- df %>% filter(station == "Göteborg") %>% arrange(lat_name, date)
malmo <- df %>% filter(station == "Malmö") %>% arrange(lat_name, date)

translation <- data.frame("Latin" = c("Alnus", "Betula", "Corylus", "Poaceae", "Quercus", "Salix", "Ulmus"), 
                          "English" = c("Alder", "Birch", "Hazel", "Grass", "Oak", "Willow", "Elm"), 
                          "Swedish" = c("Al", "Björk", "Hassel", "Gräs", "Ek", "Viden", "Alm"))

colnames(translation) <- c("Latin name", "English name", "Swedish name")

