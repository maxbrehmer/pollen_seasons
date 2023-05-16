# Model: Linear regression on quantiles

# Creating vector for reweighting
count_per_year <- df %>% group_by(lat_name, station) %>% count(Year)

new_count <- count_per_year %>%
  group_by(lat_name, station) %>%
  summarise(n_list = list(n))

# 1st quantile
data_q1 <- df %>%
  group_by(Year, lat_name, station, latitude) %>%
  summarise(q1 = quantile(greg_day, prob = .01))

eq_1 <- data_q1 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>%
  left_join(new_count, by = c("lat_name", "station")) %>%
  mutate(model = list(lm(formula = q1 ~ Year, data = data, weights = n_list))) %>%
  mutate(coeff = summary(model)$coefficients["Year", "Estimate"], 
         "P value" = summary(model)$coefficients["Year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(`Predicted 2050 1% (EQ)` = predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 77)),
         `Predicted 2023 1% (EQ)` = predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 50)), 
         `Predicted 1973 1% (EQ)` = predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 0))) %>%
  dplyr::select(c(-data))


# 50th quantile (median)
data_q50 <- df %>%
  group_by(Year, lat_name, station, latitude) %>%
  summarise(q50 = quantile(greg_day, prob = .5))

eq_50 <- data_q50 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>%
  left_join(new_count, by = c("lat_name", "station")) %>%
  mutate(model = list(lm(formula = q50 ~ Year, data = data, weights = n_list))) %>%
  mutate(coeff = summary(model)$coefficients["Year", "Estimate"], 
         "P value" = summary(model)$coefficients["Year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(`Predicted 2050 50% (EQ)` = predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 77)),
         `Predicted 2023 50% (EQ)` = predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 50)), 
         `Predicted 1973 50% (EQ)` = predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 0))) %>%
  dplyr::select(c(-data))


# 95th quantile
data_q95 <- df %>%
  group_by(Year, lat_name, station, latitude) %>%
  summarise(q95 = quantile(greg_day, prob = .95))

eq_95 <- data_q95 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>%
  left_join(new_count, by = c("lat_name", "station")) %>%
  mutate(model = list(lm(formula = q95 ~ Year, data = data, weights = n_list))) %>%
  mutate(coeff = summary(model)$coefficients["Year", "Estimate"], 
         "P value" = summary(model)$coefficients["Year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(`Predicted 2050 95% (EQ)` = predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 77)),
         `Predicted 2023 95% (EQ)` = predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 50)), 
         `Predicted 1973 95% (EQ)` = predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 0))) %>%
  dplyr::select(c(-data))


# Joining together the relevant information from each table
eq_joined <- eq_1 %>%
  dplyr::select(lat_name, station, latitude, coeff, `P value`, `Adj. R-squared`, `Predicted 1973 1% (EQ)`, `Predicted 2023 1% (EQ)`, `Predicted 2050 1% (EQ)`) %>%
  rename("coefficient 1%" = coeff, "P value 1%" = `P value`, "Adj. R^2 1%" = `Adj. R-squared`) %>%
  left_join(eq_50 %>% rename("coefficient 50%" = coeff, "P value 50%" = `P value`, "Adj. R^2 50%" = `Adj. R-squared`) %>% 
              dplyr::select(lat_name, station, latitude, "coefficient 50%", "P value 50%", "Adj. R^2 50%", `Predicted 1973 50% (EQ)`, `Predicted 2023 50% (EQ)`, `Predicted 2050 50% (EQ)`),
            by = c("lat_name", "station", "latitude")) %>%
  left_join(eq_95 %>% rename("coefficient 95%" = coeff, "P value 95%" = `P value`, "Adj. R^2 95%" = `Adj. R-squared`) %>% 
              dplyr::select(lat_name, station, latitude, "coefficient 95%", "P value 95%", "Adj. R^2 95%", `Predicted 1973 95% (EQ)`, `Predicted 2023 95% (EQ)`, `Predicted 2050 95% (EQ)`),
            by = c("lat_name", "station", "latitude")) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

##############################################
##############################################

# Tables for visualizing the EQ models
joined_eq_1 <- eq_joined %>% arrange(`coefficient 1%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, `Adj. R^2 1%`, `Predicted 1973 1% (EQ)`, `Predicted 2023 1% (EQ)`, `Predicted 2050 1% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", 
         "P value (EQ)" = "P value 1%", "Adj. R^2 (EQ)" = "Adj. R^2 1%",
         "Predicted 1973 (EQ)" = "Predicted 1973 1% (EQ)", "Predicted 2023 (EQ)" = "Predicted 2023 1% (EQ)", "Predicted 2050 (EQ)" = "Predicted 2050 1% (EQ)")

joined_eq_50 <- eq_joined %>% arrange(`coefficient 50%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, `Adj. R^2 50%`, `Predicted 1973 50% (EQ)`, `Predicted 2023 50% (EQ)`, `Predicted 2050 50% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", 
         "P value (EQ)" = "P value 50%", "Adj. R^2 (EQ)" = "Adj. R^2 50%",
         "Predicted 1973 (EQ)" = "Predicted 1973 50% (EQ)", "Predicted 2023 (EQ)" = "Predicted 2023 50% (EQ)", "Predicted 2050 (EQ)" = "Predicted 2050 50% (EQ)")

joined_eq_95 <- eq_joined %>% arrange(`coefficient 95%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, `Adj. R^2 95%`, `Predicted 1973 95% (EQ)`, `Predicted 2023 95% (EQ)`, `Predicted 2050 95% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", 
         "P value (EQ)" = "P value 95%", "Adj. R^2 (EQ)" = "Adj. R^2 95%",
         "Predicted 1973 (EQ)" = "Predicted 1973 95% (EQ)", "Predicted 2023 (EQ)" = "Predicted 2023 95% (EQ)", "Predicted 2050 (EQ)" = "Predicted 2050 95% (EQ)")

################################################################################
################################# ALL STATIONS #################################
################################################################################

# Season shift tables per species

shift_eq_1 <- joined_eq_1 %>%
  group_by(Species) %>%
  #filter(`P value (EQ)` <= 0.05) %>%
  mutate(`Avg slope (1%)` = round(mean(`Coefficient (EQ)`), 3), 
         `Adj. R^2 (1%)` = round(mean(`Adj. R^2 (EQ)`), 2), 
         `P-value (1%)` = round(mean(`P value (EQ)`), 2), 
         `Estimated start 1973` = round(mean(`Predicted 1973 (EQ)`)), 
         `Estimated start 2023` = round(mean(`Predicted 2023 (EQ)`)), 
         `Estimated start 2050` = round(mean(`Predicted 2050 (EQ)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Avg slope (1%)`, `P-value (1%)`, `Adj. R^2 (1%)`, `Estimated start 1973`, `Estimated start 2023`, `Estimated start 2050`)

shift_eq_50 <- joined_eq_50 %>%
  group_by(Species) %>%
  #filter(`P value (EQ)` <= 0.05) %>%
  mutate(`Avg slope (50%)` = round(mean(`Coefficient (EQ)`), 3), 
         `Adj. R^2 (50%)` = round(mean(`Adj. R^2 (EQ)`), 2), 
         `P-value (50%)` = round(mean(`P value (EQ)`), 2), 
         `Estimated peak 1973` = round(mean(`Predicted 1973 (EQ)`)), 
         `Estimated peak 2023` = round(mean(`Predicted 2023 (EQ)`)), 
         `Estimated peak 2050` = round(mean(`Predicted 2050 (EQ)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Avg slope (50%)`, `P-value (50%)`, `Adj. R^2 (50%)`, `Estimated peak 1973`, `Estimated peak 2023`, `Estimated peak 2050`)

shift_eq_95 <- joined_eq_95 %>%
  group_by(Species) %>%
  #filter(`P value (EQ)` <= 0.05) %>%
  mutate(`Avg slope (95%)` = round(mean(`Coefficient (EQ)`), 3), 
         `Adj. R^2 (95%)` = round(mean(`Adj. R^2 (EQ)`), 2), 
         `P-value (95%)` = round(mean(`P value (EQ)`), 2), 
         `Estimated end 1973` = round(mean(`Predicted 1973 (EQ)`)), 
         `Estimated end 2023` = round(mean(`Predicted 2023 (EQ)`)), 
         `Estimated end 2050` = round(mean(`Predicted 2050 (EQ)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Avg slope (95%)`, `P-value (95%)`, `Adj. R^2 (95%)`, `Estimated end 1973`, `Estimated end 2023`, `Estimated end 2050`)

# Joined table for season lengths

shift_by_species <- shift_eq_1 %>%
  left_join(shift_eq_50, by = c("Species")) %>%
  left_join(shift_eq_95, by = c("Species")) %>%
  mutate(`Season length 1973` = abs(`Estimated end 1973` - `Estimated start 1973`), 
         `Season length 2023` = abs(`Estimated end 2023` - `Estimated start 2023`),
         `Season length 2050` = abs(`Estimated end 2050` - `Estimated start 2050`)) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated start 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Estimated start 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Estimated start 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated start 1973` = paste0(month_1973, " ", day_1973), 
         `Estimated start 2023` = paste0(month_2023, " ", day_2023), 
         `Estimated start 2050` = paste0(month_2050, " ", day_2050), 
         `Slope (season length)` = round((`Season length 2023`-`Season length 1973`), 3)/50) %>%
  dplyr::select(Species, `Avg slope (1%)`, `Avg slope (50%)`, `Avg slope (95%)`,
                `P-value (1%)`, `P-value (50%)`, `P-value (95%)`, 
                `Adj. R^2 (1%)`, `Adj. R^2 (50%)`, `Adj. R^2 (95%)`, 
                `Estimated start 1973`, `Estimated start 2023`, `Estimated start 2050`, 
                `Season length 1973`, `Season length 2023`, `Season length 2050`, 
                `Slope (season length)`) %>%
  arrange(`Avg slope (1%)`)


################################################################################
################################## STOCKHOLM ###################################
################################################################################

# Season shift tables per species (Stockholm)

shift_eq_1_sthlm <- joined_eq_1 %>%
  filter(Location == "Stockholm") %>%
  mutate(`Slope (1%)  ` = round(`Coefficient (EQ)`, 3), 
         `Adj. R^2 (1%)` = round(`Adj. R^2 (EQ)`, 2), 
         `P-value (1%)` = round(`P value (EQ)`, 2), 
         `Estimated start 1973` = `Predicted 1973 (EQ)`, 
         `Estimated start 2023` = `Predicted 2023 (EQ)`, 
         `Estimated start 2050` = `Predicted 2050 (EQ)`) %>%
  #distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Slope (1%)  `, `P-value (1%)`, `Adj. R^2 (1%)`, `Estimated start 1973`, `Estimated start 2023`, `Estimated start 2050`)

shift_eq_50_sthlm <- joined_eq_50 %>%
  filter(Location == "Stockholm") %>%
  mutate(`Slope (50%)` = round(`Coefficient (EQ)`, 3), 
         `Adj. R^2 (50%)` = round(`Adj. R^2 (EQ)`, 2), 
         `P-value (50%)` = round(`P value (EQ)`, 2), 
         `Estimated peak 1973` = `Predicted 1973 (EQ)`, 
         `Estimated peak 2023` = `Predicted 2023 (EQ)`, 
         `Estimated peak 2050` = `Predicted 2050 (EQ)`) %>%
  #distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Slope (50%)`, `P-value (50%)`, `Adj. R^2 (50%)`, `Estimated peak 1973`, `Estimated peak 2023`, `Estimated peak 2050`)

shift_eq_95_sthlm <- joined_eq_95 %>%
  filter(Location == "Stockholm") %>%
  mutate(`Slope (95%)` = round(`Coefficient (EQ)`, 3), 
         `Adj. R^2 (95%)` = round(`Adj. R^2 (EQ)`, 2), 
         `P-value (95%)` = round(`P value (EQ)`, 2), 
         `Estimated end 1973` = `Predicted 1973 (EQ)`, 
         `Estimated end 2023` = `Predicted 2023 (EQ)`, 
         `Estimated end 2050` = `Predicted 2050 (EQ)`) %>%
  #distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Slope (95%)`, `P-value (95%)`, `Adj. R^2 (95%)`, `Estimated end 1973`, `Estimated end 2023`, `Estimated end 2050`)

# Joined table for season lengths (Stockholm)

shift_by_species_sthlm <- shift_eq_1_sthlm %>%
  left_join(shift_eq_50_sthlm, by = c("Species")) %>%
  left_join(shift_eq_95_sthlm, by = c("Species")) %>%
  mutate(`Season length 1973` = round(abs(`Estimated end 1973` - `Estimated start 1973`)), 
         `Season length 2023` = round(abs(`Estimated end 2023` - `Estimated start 2023`)),
         `Season length 2050` = round(abs(`Estimated end 2050` - `Estimated start 2050`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated start 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Estimated start 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Estimated start 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated start 1973` = paste0(month_1973, " ", day_1973), 
         `Estimated start 2023` = paste0(month_2023, " ", day_2023), 
         `Estimated start 2050` = paste0(month_2050, " ", day_2050), 
         `Slope (season length)` = round((`Season length 2023`-`Season length 1973`), 3)/50) %>%
  dplyr::select(Species, `Slope (1%)  `, `Slope (50%)`, `Slope (95%)`,
                `P-value (1%)`, `P-value (50%)`, `P-value (95%)`, 
                `Adj. R^2 (1%)`, `Adj. R^2 (50%)`, `Adj. R^2 (95%)`, 
                `Estimated start 1973`, `Estimated start 2023`, `Estimated start 2050`, 
                `Season length 1973`, `Season length 2023`, `Season length 2050`, 
                `Slope (season length)`) %>%
  arrange(`Slope (1%)  `)



################################################################################
################################### APPENDIX ###################################
################################################################################

# Season shift tables per species

appendix_1 <- joined_eq_1 %>%
  mutate(`Slope  ` = round(`Coefficient (EQ)`, 3), 
         `Adj. R^2` = round(`Adj. R^2 (EQ)`, 2), 
         `P-value` = round(`P value (EQ)`, 2), 
         `Estimated start 1973` = round(mean(`Predicted 1973 (EQ)`)), 
         `Estimated start 2023` = round(mean(`Predicted 2023 (EQ)`)), 
         `Estimated start 2050` = round(mean(`Predicted 2050 (EQ)`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated start 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Estimated start 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Estimated start 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated start 1973` = paste0(month_1973, " ", day_1973), 
         `Estimated start 2023` = paste0(month_2023, " ", day_2023), 
         `Estimated start 2050` = paste0(month_2050, " ", day_2050)) %>%
  dplyr::select(Species, Location, `Slope  `, `P-value`, `Adj. R^2`, `Estimated start 1973`, `Estimated start 2023`, `Estimated start 2050`)

appendix_50 <- joined_eq_50 %>%
  mutate(`Slope  ` = round(`Coefficient (EQ)`, 3), 
         `Adj. R^2` = round(`Adj. R^2 (EQ)`, 2), 
         `P-value` = round(`P value (EQ)`, 2), 
         `Estimated peak 1973` = round(mean(`Predicted 1973 (EQ)`)), 
         `Estimated peak 2023` = round(mean(`Predicted 2023 (EQ)`)), 
         `Estimated peak 2050` = round(mean(`Predicted 2050 (EQ)`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated peak 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Estimated peak 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Estimated peak 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated peak 1973` = paste0(month_1973, " ", day_1973), 
         `Estimated peak 2023` = paste0(month_2023, " ", day_2023), 
         `Estimated peak 2050` = paste0(month_2050, " ", day_2050)) %>%
  dplyr::select(Species, Location, `Slope  `, `P-value`, `Adj. R^2`, `Estimated peak 1973`, `Estimated peak 2023`, `Estimated peak 2050`)

appendix_95 <- joined_eq_95 %>%
  mutate(`Slope  ` = round(`Coefficient (EQ)`, 3), 
         `Adj. R^2` = round(`Adj. R^2 (EQ)`, 2), 
         `P-value` = round(`P value (EQ)`, 2), 
         `Estimated end 1973` = round(mean(`Predicted 1973 (EQ)`)), 
         `Estimated end 2023` = round(mean(`Predicted 2023 (EQ)`)), 
         `Estimated end 2050` = round(mean(`Predicted 2050 (EQ)`))) %>%
  mutate(dates_1973 = as.Date(paste0("1973-", `Estimated end 1973`), format = "%Y-%j"),
         dates_2023 = as.Date(paste0("2023-", `Estimated end 2023`), format = "%Y-%j"),
         dates_2050 = as.Date(paste0("2050-", `Estimated end 2050`), format = "%Y-%j"), 
         month_1973 = format(dates_1973, "%B"), 
         month_2023 = format(dates_2023, "%B"), 
         month_2050 = format(dates_2050, "%B"), 
         day_1973 = format(dates_1973, "%e"), 
         day_2023 = format(dates_2023, "%e"), 
         day_2050 = format(dates_2050, "%e"), 
         `Estimated end 1973` = paste0(month_1973, " ", day_1973), 
         `Estimated end 2023` = paste0(month_2023, " ", day_2023), 
         `Estimated end 2050` = paste0(month_2050, " ", day_2050)) %>%
  dplyr::select(Species, Location, `Slope  `, `P-value`, `Adj. R^2`, `Estimated end 1973`, `Estimated end 2023`, `Estimated end 2050`)






