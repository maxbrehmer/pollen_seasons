
# Adding regression coefficients to each table
qr_1 <- qr_1 %>% mutate("coefficient 1% (QR)" = model$coefficients["Year"])
qr_50 <- qr_50 %>% mutate("coefficient 50% (QR)" = model$coefficients["Year"])
qr_95 <- qr_95 %>% mutate("coefficient 95% (QR)" = model$coefficients["Year"])

# Joining EQ and QR tables
joined <- eq_joined %>% left_join(qr_1 %>% dplyr::select(c(-model)), by = c("lat_name", "station")) %>%
  left_join(qr_50 %>% dplyr::select(c(-model)), by = c("lat_name", "station")) %>%
  left_join(qr_95 %>% dplyr::select(c(-model)), by = c("lat_name", "station")) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Tables for visualizing the EQ models
joined_eq_1 <- joined %>% arrange(`coefficient 1%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, `coefficient 1% (QR)`, 
                `Predicted 1973 1% (EQ)`, `Predicted 2023 1% (EQ)`, `Predicted 2050 1% (EQ)`, `Predicted 1973 1% (QR)`, `Predicted 2023 1% (QR)`, `Predicted 2050 1% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", 
         "P value (EQ)" = "P value 1%", "Coefficient (QR)" = "coefficient 1% (QR)", 
         "Predicted 1973 (EQ)" = "Predicted 1973 1% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 1% (QR)", 
         "Predicted 2023 (EQ)" = "Predicted 2023 1% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 1% (QR)",
         "Predicted 2050 (EQ)" = "Predicted 2050 1% (EQ)", "Predicted 2050 (QR)" = "Predicted 2050 1% (QR)")

joined_eq_50 <- joined %>% arrange(`coefficient 50%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, `coefficient 50% (QR)`, 
                `Predicted 1973 50% (EQ)`, `Predicted 2023 50% (EQ)`, `Predicted 2050 50% (EQ)`, `Predicted 1973 50% (QR)`, `Predicted 2023 50% (QR)`, `Predicted 2050 50% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", 
         "P value (EQ)" = "P value 50%", "Coefficient (QR)" = "coefficient 50% (QR)", 
         "Predicted 1973 (EQ)" = "Predicted 1973 50% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 50% (QR)", 
         "Predicted 2023 (EQ)" = "Predicted 2023 50% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 50% (QR)",
         "Predicted 2050 (EQ)" = "Predicted 2050 50% (EQ)", "Predicted 2050 (QR)" = "Predicted 2050 50% (QR)")

joined_eq_95 <- joined %>% arrange(`coefficient 95%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, `coefficient 95% (QR)`, 
                `Predicted 1973 95% (EQ)`, `Predicted 2023 95% (EQ)`, `Predicted 2050 95% (EQ)`, `Predicted 1973 95% (QR)`, `Predicted 2023 95% (QR)`, `Predicted 2050 95% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", 
         "P value (EQ)" = "P value 95%", "Coefficient (QR)" = "coefficient 95% (QR)", 
         "Predicted 1973 (EQ)" = "Predicted 1973 95% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 95% (QR)", 
         "Predicted 2023 (EQ)" = "Predicted 2023 95% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 95% (QR)",
         "Predicted 2050 (EQ)" = "Predicted 2050 95% (EQ)", "Predicted 2050 (QR)" = "Predicted 2050 95% (QR)")

# Tables for visualizing the QR models
joined_qr_1 <- joined %>% arrange(`coefficient 1% (QR)`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, `coefficient 1% (QR)`, 
                `Predicted 1973 1% (EQ)`, `Predicted 1973 1% (QR)`, `Predicted 2023 1% (EQ)`, `Predicted 2023 1% (QR)`, `Predicted 2050 1% (EQ)`, `Predicted 2050 1% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", 
         "P value (EQ)" = "P value 1%", "Coefficient (QR)" = "coefficient 1% (QR)", 
         "Predicted 1973 (EQ)" = "Predicted 1973 1% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 1% (QR)", 
         "Predicted 2023 (EQ)" = "Predicted 2023 1% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 1% (QR)", 
         "Predicted 2050 (EQ)" = "Predicted 2050 1% (EQ)", "Predicted 2050 (QR)" = "Predicted 2050 1% (QR)")

joined_qr_50 <- joined %>% arrange(`coefficient 50% (QR)`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, `coefficient 50% (QR)`, 
                `Predicted 1973 50% (EQ)`, `Predicted 1973 50% (QR)`, `Predicted 2023 50% (EQ)`, `Predicted 2023 50% (QR)`, `Predicted 2050 50% (EQ)`, `Predicted 2050 50% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", 
         "P value (EQ)" = "P value 50%", "Coefficient (QR)" = "coefficient 50% (QR)", 
         "Predicted 1973 (EQ)" = "Predicted 1973 50% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 50% (QR)", 
         "Predicted 2023 (EQ)" = "Predicted 2023 50% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 50% (QR)", 
         "Predicted 2050 (EQ)" = "Predicted 2050 50% (EQ)", "Predicted 2050 (QR)" = "Predicted 2050 50% (QR)")

joined_qr_95 <- joined %>% arrange(`coefficient 95% (QR)`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, `coefficient 95% (QR)`, 
                `Predicted 1973 95% (EQ)`, `Predicted 1973 95% (QR)`, `Predicted 2023 95% (EQ)`, `Predicted 2023 95% (QR)`, `Predicted 2050 95% (EQ)`, `Predicted 2050 95% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", 
         "P value (EQ)" = "P value 95%", "Coefficient (QR)" = "coefficient 95% (QR)", 
         "Predicted 1973 (EQ)" = "Predicted 1973 95% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 95% (QR)", 
         "Predicted 2023 (EQ)" = "Predicted 2023 95% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 95% (QR)", 
         "Predicted 2050 (EQ)" = "Predicted 2050 95% (EQ)", "Predicted 2050 (QR)" = "Predicted 2050 95% (QR)")


# Season shift tables per species

shift_qr_1 <- joined_qr_1 %>%
  group_by(Species) %>%
  mutate(`Avg slope (1%)` = round(mean(`Coefficient (QR)`), 3), 
         #`Adj. R^2 (1%)` = round(mean(`Adj. R^2 (QR)`), 2), 
         #`P-value (1%)` = round(mean(`P value (QR)`), 2), 
         `Estimated start 1973` = round(mean(`Predicted 1973 (QR)`)), 
         `Predicted start 2023` = round(mean(`Predicted 2023 (QR)`)), 
         `Predicted start 2050` = round(mean(`Predicted 2050 (QR)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Avg slope (1%)`, `Estimated start 1973`, `Predicted start 2023`, `Predicted start 2050`)

shift_qr_50 <- joined_qr_50 %>%
  group_by(Species) %>%
  mutate(`Avg slope (50%)` = round(mean(`Coefficient (QR)`), 3), 
         #`Adj. R^2 (50%)` = round(mean(`Adj. R^2 (EQ)`), 2), 
         #`P-value (50%)` = round(mean(`P value (EQ)`), 2), 
         `Estimated peak 1973` = round(mean(`Predicted 1973 (QR)`)), 
         `Predicted peak 2023` = round(mean(`Predicted 2023 (QR)`)), 
         `Predicted peak 2050` = round(mean(`Predicted 2050 (QR)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Avg slope (50%)`, `Estimated peak 1973`, `Predicted peak 2023`, `Predicted peak 2050`)

shift_qr_95 <- joined_qr_95 %>%
  group_by(Species) %>%
  mutate(`Avg slope (95%)` = round(mean(`Coefficient (QR)`), 3), 
         #`Adj. R^2 (95%)` = round(mean(`Adj. R^2 (EQ)`), 2), 
         #`P-value (95%)` = round(mean(`P value (EQ)`), 2), 
         `Estimated end 1973` = round(mean(`Predicted 1973 (QR)`)), 
         `Predicted end 2023` = round(mean(`Predicted 2023 (QR)`)), 
         `Predicted end 2050` = round(mean(`Predicted 2050 (QR)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Avg slope (95%)`, `Estimated end 1973`, `Predicted end 2023`, `Predicted end 2050`)

# Joined table for season lengths

shift_by_species <- shift_qr_1 %>%
  left_join(shift_qr_50, by = c("Species")) %>%
  left_join(shift_qr_95, by = c("Species")) %>%
  mutate(`Season length 1973` = abs(`Estimated end 1973` - `Estimated start 1973`), 
         `Season length 2023` = abs(`Predicted end 2023` - `Predicted start 2023`),
         `Season length 2050` = abs(`Predicted end 2050` - `Predicted start 2050`)) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated start 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Predicted start 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Predicted start 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated start 1973` = paste0(month_1973, " ", day_1973), 
         `Predicted start 2023` = paste0(month_2023, " ", day_2023), 
         `Predicted start 2050` = paste0(month_2050, " ", day_2050), 
         `Slope (season length)` = round((`Season length 2023`-`Season length 1973`), 3)/50) %>%
  dplyr::select(Species, `Avg slope (1%)`, `Avg slope (50%)`, `Avg slope (95%)`,
                #`P-value (1%)`, `P-value (50%)`, `P-value (95%)`, 
                #`Adj. R^2 (1%)`, `Adj. R^2 (50%)`, `Adj. R^2 (95%)`, 
                `Estimated start 1973`, `Predicted start 2023`, `Predicted start 2050`, 
                `Season length 1973`, `Season length 2023`, `Season length 2050`, 
                `Slope (season length)`) %>%
  arrange(`Avg slope (1%)`)


################################################################################
################################## STOCKHOLM ###################################
################################################################################

# Season shift tables per species (Stockholm)

shift_qr_1_sthlm <- joined_qr_1 %>%
  filter(Location == "Stockholm") %>%
  mutate(`Slope (1%)  ` = round(`Coefficient (QR)`, 3), 
         #`Adj. R^2 (1%)` = round(`Adj. R^2 (QR)`, 2), 
         #`P-value (1%)` = round(`P value (QR)`, 2), 
         `Estimated start 1973` = `Predicted 1973 (QR)`, 
         `Predicted start 2023` = `Predicted 2023 (QR)`, 
         `Predicted start 2050` = `Predicted 2050 (QR)`) %>%
  #distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Slope (1%)  `, `Estimated start 1973`, `Predicted start 2023`, `Predicted start 2050`)

shift_qr_50_sthlm <- joined_qr_50 %>%
  filter(Location == "Stockholm") %>%
  mutate(`Slope (50%)` = round(`Coefficient (QR)`, 3), 
         #`Adj. R^2 (50%)` = round(`Adj. R^2 (QR)`, 2), 
         #`P-value (50%)` = round(`P value (QR)`, 2), 
         `Estimated peak 1973` = `Predicted 1973 (QR)`, 
         `Predicted peak 2023` = `Predicted 2023 (QR)`, 
         `Predicted peak 2050` = `Predicted 2050 (QR)`) %>%
  #distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Slope (50%)`, `Estimated peak 1973`, `Predicted peak 2023`, `Predicted peak 2050`)

shift_qr_95_sthlm <- joined_qr_95 %>%
  filter(Location == "Stockholm") %>%
  mutate(`Slope (95%)` = round(`Coefficient (QR)`, 3), 
         #`Adj. R^2 (95%)` = round(`Adj. R^2 (QR)`, 2), 
         #`P-value (95%)` = round(`P value (QR)`, 2), 
         `Estimated end 1973` = `Predicted 1973 (QR)`, 
         `Predicted end 2023` = `Predicted 2023 (QR)`, 
         `Predicted end 2050` = `Predicted 2050 (QR)`) %>%
  #distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Slope (95%)`, `Estimated end 1973`, `Predicted end 2023`, `Predicted end 2050`)

# Joined table for season lengths (Stockholm)

shift_by_species_sthlm <- shift_qr_1_sthlm %>%
  left_join(shift_qr_50_sthlm, by = c("Species")) %>%
  left_join(shift_qr_95_sthlm, by = c("Species")) %>%
  mutate(`Season length 1973` = round(abs(`Estimated end 1973` - `Estimated start 1973`)), 
         `Season length 2023` = round(abs(`Predicted end 2023` - `Predicted start 2023`)),
         `Season length 2050` = round(abs(`Predicted end 2050` - `Predicted start 2050`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated start 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Predicted start 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Predicted start 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated start 1973` = paste0(month_1973, " ", day_1973), 
         `Predicted start 2023` = paste0(month_2023, " ", day_2023), 
         `Predicted start 2050` = paste0(month_2050, " ", day_2050), 
         `Slope (season length)` = round((`Season length 2023`-`Season length 1973`), 3)/50) %>%
  dplyr::select(Species, `Slope (1%)  `, `Slope (50%)`, `Slope (95%)`,
                #`P-value (1%)`, `P-value (50%)`, `P-value (95%)`, 
                #`Adj. R^2 (1%)`, `Adj. R^2 (50%)`, `Adj. R^2 (95%)`, 
                `Estimated start 1973`, `Predicted start 2023`, `Predicted start 2050`, 
                `Season length 1973`, `Season length 2023`, `Season length 2050`, 
                `Slope (season length)`) %>%
  arrange(`Slope (1%)  `)



################################################################################
################################### APPENDIX ###################################
################################################################################

# Season shift tables per species

appendix_1 <- joined_qr_1 %>%
  mutate(`Slope  ` = round(`Coefficient (QR)`, 3), 
         #`Adj. R^2` = round(`Adj. R^2 (QR)`, 2), 
         #`P-value` = round(`P value (QR)`, 2), 
         `Estimated start 1973` = round(mean(`Predicted 1973 (QR)`)), 
         `Predicted start 2023` = round(mean(`Predicted 2023 (QR)`)), 
         `Predicted start 2050` = round(mean(`Predicted 2050 (QR)`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated start 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Predicted start 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Predicted start 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated start 1973` = paste0(month_1973, " ", day_1973), 
         `Predicted start 2023` = paste0(month_2023, " ", day_2023), 
         `Predicted start 2050` = paste0(month_2050, " ", day_2050)) %>%
  dplyr::select(Species, Location, `Slope  `, `Estimated start 1973`, `Predicted start 2023`, `Predicted start 2050`)

appendix_50 <- joined_qr_50 %>%
  mutate(`Slope  ` = round(`Coefficient (QR)`, 3), 
         #`Adj. R^2` = round(`Adj. R^2 (QR)`, 2), 
         #`P-value` = round(`P value (QR)`, 2), 
         `Estimated peak 1973` = round(mean(`Predicted 1973 (QR)`)), 
         `Predicted peak 2023` = round(mean(`Predicted 2023 (QR)`)), 
         `Predicted peak 2050` = round(mean(`Predicted 2050 (QR)`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated peak 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Predicted peak 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Predicted peak 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated peak 1973` = paste0(month_1973, " ", day_1973), 
         `Predicted peak 2023` = paste0(month_2023, " ", day_2023), 
         `Predicted peak 2050` = paste0(month_2050, " ", day_2050)) %>%
  dplyr::select(Species, Location, `Slope  `, `Estimated peak 1973`, `Predicted peak 2023`, `Predicted peak 2050`)

appendix_95 <- joined_qr_95 %>%
  mutate(`Slope  ` = round(`Coefficient (QR)`, 3), 
         #`Adj. R^2` = round(`Adj. R^2 (QR)`, 2), 
         #`P-value` = round(`P value (QR)`, 2), 
         `Estimated end 1973` = round(mean(`Predicted 1973 (QR)`)), 
         `Predicted end 2023` = round(mean(`Predicted 2023 (QR)`)), 
         `Predicted end 2050` = round(mean(`Predicted 2050 (QR)`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated end 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Predicted end 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Predicted end 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated end 1973` = paste0(month_1973, " ", day_1973), 
         `Predicted end 2023` = paste0(month_2023, " ", day_2023), 
         `Predicted end 2050` = paste0(month_2050, " ", day_2050)) %>%
  dplyr::select(Species, Location, `Slope  `, `Estimated end 1973`, `Predicted end 2023`, `Predicted end 2050`)


