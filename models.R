# Setting years before 2023 as starting at 2023 and going backwards
df <- df %>% mutate(Year = year - 1973)



# Model: Linear regression on quantiles

# 1st quantile
data_q1 <- df %>%
  group_by(Year, lat_name, station, latitude) %>%
  summarise(q1 = quantile(greg_day, prob = .01))

weights_q1 <- data_q1 %>%
  group_by(lat_name, station) %>% 
  summarise(n = n()) %>%
  ungroup()

eq_1 <- data_q1 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>%
  left_join(weights_q1, by = c("lat_name", "station")) %>% mutate(weight = 1/n) %>%

  mutate(model = list(lm(formula = q1 ~ Year, data = data, weights = rep(weight, times = n)))) %>%
  mutate(coeff = summary(model)$coefficients["Year", "Estimate"], 
         "P value" = summary(model)$coefficients["Year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(`Predicted 2023 1% (EQ)` = as.integer(predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 50))), 
         `Predicted 1973 1% (EQ)` = as.integer(predict(lm(q1 ~ Year, data = data), newdata = data.frame(Year = 0)))) %>%
  dplyr::select(c(-data))


# 50th quantile (median)
data_q50 <- df %>%
  group_by(Year, lat_name, station, latitude) %>%
  summarise(q50 = quantile(greg_day, prob = .5))

weights_q50 <- data_q50 %>%
  group_by(lat_name, station) %>% 
  summarise(n = n()) %>%
  ungroup()

eq_50 <- data_q50 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>%
  left_join(weights_q50, by = c("lat_name", "station")) %>% mutate(weight = 1/n) %>%
  
  mutate(model = list(lm(formula = q50 ~ Year, data = data, weights = rep(weight, times = n)))) %>%
  mutate(coeff = summary(model)$coefficients["Year", "Estimate"], 
         "P value" = summary(model)$coefficients["Year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(`Predicted 2023 50% (EQ)` = as.integer(predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 50))), 
         `Predicted 1973 50% (EQ)` = as.integer(predict(lm(q50 ~ Year, data = data), newdata = data.frame(Year = 0)))) %>%
  dplyr::select(c(-data))


# 95th quantile
data_q95 <- df %>%
  group_by(Year, lat_name, station, latitude) %>%
  summarise(q95 = quantile(greg_day, prob = .95))

weights_q95 <- data_q95 %>%
  group_by(lat_name, station) %>% 
  summarise(n = n()) %>%
  ungroup()

eq_95 <- data_q95 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>%
  left_join(weights_q95, by = c("lat_name", "station")) %>% mutate(weight = 1/n) %>%
  
  mutate(model = list(lm(formula = q95 ~ Year, data = data, weights = rep(weight, times = n)))) %>%
  mutate(coeff = summary(model)$coefficients["Year", "Estimate"], 
         "P value" = summary(model)$coefficients["Year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(`Predicted 2023 95% (EQ)` = as.integer(predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 50))), 
         `Predicted 1973 95% (EQ)` = as.integer(predict(lm(q95 ~ Year, data = data), newdata = data.frame(Year = 0)))) %>%
  dplyr::select(c(-data))


# Joining together the relevant information from each table
eq_joined <- eq_1 %>%
  dplyr::select(lat_name, station, latitude, coeff, `P value`, `Predicted 2023 1% (EQ)`, `Predicted 1973 1% (EQ)`) %>%
  rename("coefficient 1%" = coeff, "P value 1%" = `P value`) %>%
  left_join(eq_50 %>% rename("coefficient 50%" = coeff, "P value 50%" = `P value`) %>% dplyr::select(lat_name, station, latitude, "coefficient 50%", "P value 50%", `Predicted 2023 50% (EQ)`, `Predicted 1973 50% (EQ)`),
            by = c("lat_name", "station", "latitude")) %>%
  left_join(eq_95 %>% rename("coefficient 95%" = coeff, "P value 95%" = `P value`) %>% dplyr::select(lat_name, station, latitude, "coefficient 95%", "P value 95%", `Predicted 2023 95% (EQ)`, `Predicted 1973 95% (EQ)`),
            by = c("lat_name", "station", "latitude")) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

##############################################
##############################################

# Tables for visualizing the EQ models
joined_eq_1 <- eq_joined %>% arrange(`coefficient 1%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, `Predicted 2023 1% (EQ)`, `Predicted 1973 1% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", 
         "P value (EQ)" = "P value 1%", "Predicted 2023 (EQ)" = "Predicted 2023 1% (EQ)", "Predicted 1973 (EQ)" = "Predicted 1973 1% (EQ)")

joined_eq_50 <- eq_joined %>% arrange(`coefficient 50%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, `Predicted 2023 50% (EQ)`, `Predicted 1973 50% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", 
         "P value (EQ)" = "P value 50%", "Predicted 2023 (EQ)" = "Predicted 2023 50% (EQ)", "Predicted 1973 (EQ)" = "Predicted 1973 50% (EQ)")

joined_eq_95 <- eq_joined %>% arrange(`coefficient 95%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, `Predicted 2023 95% (EQ)`, `Predicted 1973 95% (EQ)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", 
         "P value (EQ)" = "P value 95%", "Predicted 2023 (EQ)" = "Predicted 2023 95% (EQ)", "Predicted 1973 (EQ)" = "Predicted 1973 95% (EQ)")



