# Model: Linear regression on quantiles

# Creating vector for reweighting
count_per_year <- df %>% group_by(lat_name, station) %>% count(Year)

new_count <- count_per_year %>%
  group_by(lat_name, station) %>%
  summarize(n_list = list(n))

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
  mutate(`Predicted 2050 1% (EQ)` = as.integer(predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 77))),
         `Predicted 2023 1% (EQ)` = as.integer(predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 50))), 
         `Predicted 1973 1% (EQ)` = as.integer(predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 0)))) %>%
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
  mutate(`Predicted 2050 50% (EQ)` = as.integer(predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 77))),
         `Predicted 2023 50% (EQ)` = as.integer(predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 50))), 
         `Predicted 1973 50% (EQ)` = as.integer(predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 0)))) %>%
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
  mutate(`Predicted 2050 95% (EQ)` = as.integer(predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 77))),
         `Predicted 2023 95% (EQ)` = as.integer(predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 50))), 
         `Predicted 1973 95% (EQ)` = as.integer(predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 0)))) %>%
  dplyr::select(c(-data))


# Joining together the relevant information from each table
eq_joined <- eq_1 %>%
  dplyr::select(lat_name, station, latitude, coeff, `P value`, `Predicted 1973 1% (EQ)`, `Predicted 2023 1% (EQ)`, `Predicted 2050 1% (EQ)`) %>%
  rename("coefficient 1%" = coeff, "P value 1%" = `P value`) %>%
  left_join(eq_50 %>% rename("coefficient 50%" = coeff, "P value 50%" = `P value`) %>% 
              dplyr::select(lat_name, station, latitude, "coefficient 50%", "P value 50%", `Predicted 1973 50% (EQ)`, `Predicted 2023 50% (EQ)`, `Predicted 2050 50% (EQ)`),
            by = c("lat_name", "station", "latitude")) %>%
  left_join(eq_95 %>% rename("coefficient 95%" = coeff, "P value 95%" = `P value`) %>% 
              dplyr::select(lat_name, station, latitude, "coefficient 95%", "P value 95%", `Predicted 1973 95% (EQ)`, `Predicted 2023 95% (EQ)`, `Predicted 2050 95% (EQ)`),
            by = c("lat_name", "station", "latitude")) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

##############################################
##############################################

# Tables for visualizing the EQ models
joined_eq_1 <- eq_joined %>% arrange(`coefficient 1%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, `Predicted 1973 1% (EQ)`, `Predicted 2023 1% (EQ)`, `Predicted 2050 1% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", 
         "P value (EQ)" = "P value 1%", "Predicted 1973 (EQ)" = "Predicted 1973 1% (EQ)", "Predicted 2023 (EQ)" = "Predicted 2023 1% (EQ)", "Predicted 2050 (EQ)" = "Predicted 2050 1% (EQ)")

joined_eq_50 <- eq_joined %>% arrange(`coefficient 50%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, `Predicted 1973 50% (EQ)`, `Predicted 2023 50% (EQ)`, `Predicted 2050 50% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", 
         "P value (EQ)" = "P value 50%", "Predicted 1973 (EQ)" = "Predicted 1973 50% (EQ)", "Predicted 2023 (EQ)" = "Predicted 2023 50% (EQ)", "Predicted 2050 (EQ)" = "Predicted 2050 50% (EQ)")

joined_eq_95 <- eq_joined %>% arrange(`coefficient 95%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, `Predicted 1973 95% (EQ)`, `Predicted 2023 95% (EQ)`, `Predicted 2050 95% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", 
         "P value (EQ)" = "P value 95%", "Predicted 1973 (EQ)" = "Predicted 1973 95% (EQ)", "Predicted 2023 (EQ)" = "Predicted 2023 95% (EQ)", "Predicted 2050 (EQ)" = "Predicted 2050 95% (EQ)")


# Season shift tables per species

shift_eq_1 <- joined_eq_1 %>%
  group_by(Species) %>%
  mutate(`Coefficient (1%)` = mean(`Coefficient (EQ)`), `Estimated day 1973 (1%)` = as.integer(mean(`Predicted 1973 (EQ)`)), 
         `Estimated day 2023 (1%)` = as.integer(mean(`Predicted 2023 (EQ)`)), `Estimated day 2050 (1%)` = as.integer(mean(`Predicted 2050 (EQ)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Coefficient (1%)`, `Estimated day 1973 (1%)`, `Estimated day 2023 (1%)`, `Estimated day 2050 (1%)`)

shift_eq_50 <- joined_eq_50 %>%
  group_by(Species) %>%
  mutate(`Coefficient (50%)` = mean(`Coefficient (EQ)`), `Estimated day 1973 (50%)` = as.integer(mean(`Predicted 1973 (EQ)`)), 
         `Estimated day 2023 (50%)` = as.integer(mean(`Predicted 2023 (EQ)`)), `Estimated day 2050 (50%)` = as.integer(mean(`Predicted 2050 (EQ)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Coefficient (50%)`, `Estimated day 1973 (50%)`, `Estimated day 2023 (50%)`, `Estimated day 2050 (50%)`)

shift_eq_95 <- joined_eq_95 %>%
  group_by(Species) %>%
  mutate(`Coefficient (95%)` = mean(`Coefficient (EQ)`), `Estimated day 1973 (95%)` = as.integer(mean(`Predicted 1973 (EQ)`)), 
         `Estimated day 2023 (95%)` = as.integer(mean(`Predicted 2023 (EQ)`)), `Estimated day 2050 (95%)` = as.integer(mean(`Predicted 2050 (EQ)`))) %>%
  distinct(Species, .keep_all = TRUE) %>%  # Keep only one row for each Species, based on all columns
  dplyr::select(Species, `Coefficient (95%)`, `Estimated day 1973 (95%)`, `Estimated day 2023 (95%)`, `Estimated day 2050 (95%)`)

# Joined table for season lengths

shift_by_species <- shift_eq_1 %>%
  left_join(shift_eq_50, by = c("Species")) %>%
  left_join(shift_eq_95, by = c("Species")) %>%
  mutate(`Season length 1973` = abs(`Estimated day 1973 (95%)` - `Estimated day 1973 (1%)`), 
         `Season length 2023` = abs(`Estimated day 2023 (95%)` - `Estimated day 2023 (1%)`),
         `Season length 2050` = abs(`Estimated day 2050 (95%)` - `Estimated day 2050 (1%)`)) %>%
  dplyr::select(Species, `Coefficient (1%)`, `Coefficient (50%)`, `Coefficient (95%)`, `Season length 1973`, `Season length 2023`, `Season length 2050`)

