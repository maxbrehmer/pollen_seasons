pic2022 <- df %>% filter(year == 2022) %>% ggplot(aes(x = date, y = lat_name)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_ridges() +
  facet_wrap(~ station)

pic1992 <- df %>% filter(year == 1992) %>% ggplot(aes(x = date, y = lat_name)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_ridges() +
  facet_wrap(~ station)

data_structure <- data.frame("Variable" = c("Station", "Pollen type", "Date", "Count", "Factor", "Latitude"), "Type" = c("categorical", "categorical", "continous", "continous", "continous", "continous"), "Decription" = c(
  "Geographic location of the pollen monitoring station.", "Genus of the recorded pollen counts.", "Gregorian calendar date on which the airborne pollen were registered.", 
  "Amount of individual pollen were collected.", "Reference variable for the size of the microscope used.", "Northern latitudinal cooardinates of said station."
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