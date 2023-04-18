# Model: Linear regression on quantiles

# 1st quantile
data_q1 <- df %>%
  group_by(year, lat_name, station, latitude) %>%
  summarise(q1 = quantile(greg_day, prob = .01))

eq_1 <- data_q1 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>% mutate(model = list(lm(formula = q1 ~ year, data = data))) %>%
  mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(pred_2023 = as.integer(predict(lm(q1 ~ year, data = data), newdata = data.frame(year = 2023)))) %>%
  dplyr::select(c(-data))

# 50th quantile (median)
data_q50 <- df %>%
  group_by(year, lat_name, station, latitude) %>%
  summarise(q50 = quantile(greg_day, prob = .5))

eq_50 <- data_q50 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>% mutate(model = list(lm(formula = q50 ~ year, data = data))) %>%
  mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(pred_2023 = as.integer(predict(lm(q50 ~ year, data = data), newdata = data.frame(year = 2023)))) %>%
  dplyr::select(c(-data))

# 95th quantile
data_q95 <- df %>%
  group_by(year, lat_name, station, latitude) %>%
  summarise(q95 = quantile(greg_day, prob = .95))

eq_95 <- data_q95 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>% mutate(model = list(lm(formula = q95 ~ year, data = data))) %>%
  mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared) %>%
  rowwise() %>%
  mutate(pred_2023 = as.integer(predict(lm(q95 ~ year, data = data), newdata = data.frame(year = 2023)))) %>%
  dplyr::select(c(-data))

# Joining together the relevant information from each table
eq_joined <- eq_1 %>%
  dplyr::select(lat_name, station, latitude, coeff, `P value`, pred_2023) %>%
  rename("coefficient 1%" = coeff, "P value 1%" = `P value`) %>%
  left_join(eq_50 %>% rename("coefficient 50%" = coeff, "P value 50%" = `P value`) %>% dplyr::select(lat_name, station, latitude, "coefficient 50%", "P value 50%", pred_2023),
            by = c("lat_name", "station", "latitude")) %>%
  left_join(eq_95 %>% rename("coefficient 95%" = coeff, "P value 95%" = `P value`) %>% dplyr::select(lat_name, station, latitude, "coefficient 95%", "P value 95%", pred_2023),
            by = c("lat_name", "station", "latitude"))

##############################################
##############################################

# Tables for visualizing the EQ models
joined_eq_1 <- eq_joined %>% arrange(`coefficient 1%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, pred_2023) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", "P value (EQ)" = "P value 1%")

joined_eq_50 <- eq_joined %>% arrange(`coefficient 50%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, pred_2023) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", "P value (EQ)" = "P value 50%")

joined_eq_95 <- eq_joined %>% arrange(`coefficient 95%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, pred_2023) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", "P value (EQ)" = "P value 95%")



