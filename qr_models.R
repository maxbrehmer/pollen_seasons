# Model: Quantile regression
qr_1 <- df %>% nest_by(station, lat_name) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.01))) %>% 
  rowwise() %>%
  mutate(`Predicted 2023 1% (QR)` = as.integer(predict(rq(greg_day ~ year, data = data, tau = 0.01), newdata = data.frame(year = 2023))), 
         `Predicted 1973 1% (QR)` = as.integer(predict(rq(greg_day ~ year, data = data, tau = 0.01), newdata = data.frame(year = 1973)))) %>%
  dplyr::select(c(-data))

qr_50 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.5))) %>% 
  rowwise() %>%
  mutate(`Predicted 2023 50% (QR)` = as.integer(predict(rq(greg_day ~ year, data = data, tau = 0.5), newdata = data.frame(year = 2023))), 
         `Predicted 1973 50% (QR)` = as.integer(predict(rq(greg_day ~ year, data = data, tau = 0.5), newdata = data.frame(year = 1973)))) %>%
  dplyr::select(c(-data))


qr_95 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.95))) %>%
  rowwise() %>%
  mutate(`Predicted 2023 95% (QR)` = as.integer(predict(rq(greg_day ~ year, data = data, tau = 0.95), newdata = data.frame(year = 2023))), 
         `Predicted 1973 95% (QR)` = as.integer(predict(rq(greg_day ~ year, data = data, tau = 0.95), newdata = data.frame(year = 1973)))) %>%
  dplyr::select(c(-data))


# Adding regression coefficients to each table
qr_1 <- qr_1 %>% mutate("coefficient 1% (QR)" = model$coefficients["year"])
qr_50 <- qr_50 %>% mutate("coefficient 50% (QR)" = model$coefficients["year"])
qr_95 <- qr_95 %>% mutate("coefficient 95% (QR)" = model$coefficients["year"])

# Joining EQ and QR tables
joined <- eq_joined %>% left_join(qr_1 %>% dplyr::select(c(-model)), by = c("lat_name", "station")) %>%
  left_join(qr_50 %>% dplyr::select(c(-model)), by = c("lat_name", "station")) %>%
  left_join(qr_95 %>% dplyr::select(c(-model)), by = c("lat_name", "station")) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Tables for visualizing the EQ models
joined_eq_1 <- joined %>% arrange(`coefficient 1%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, `coefficient 1% (QR)`, `Predicted 2023 1% (EQ)`, `Predicted 1973 1% (EQ)`, `Predicted 2023 1% (QR)`, `Predicted 1973 1% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", 
         "P value (EQ)" = "P value 1%", "Coefficient (QR)" = "coefficient 1% (QR)", "Predicted 2023 (EQ)" = "Predicted 2023 1% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 1% (QR)", "Predicted 1973 (EQ)" = "Predicted 1973 1% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 1% (QR)")

joined_eq_50 <- joined %>% arrange(`coefficient 50%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, `coefficient 50% (QR)`, `Predicted 2023 50% (EQ)`, `Predicted 1973 50% (EQ)`, `Predicted 2023 50% (QR)`, `Predicted 1973 50% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", 
         "P value (EQ)" = "P value 50%", "Coefficient (QR)" = "coefficient 50% (QR)", "Predicted 2023 (EQ)" = "Predicted 2023 50% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 50% (QR)", "Predicted 1973 (EQ)" = "Predicted 1973 50% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 50% (QR)")

joined_eq_95 <- joined %>% arrange(`coefficient 95%`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, `coefficient 95% (QR)`, `Predicted 2023 95% (EQ)`, `Predicted 1973 95% (EQ)`, `Predicted 2023 95% (QR)`, `Predicted 1973 95% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", 
         "P value (EQ)" = "P value 95%", "Coefficient (QR)" = "coefficient 95% (QR)", "Predicted 2023 (EQ)" = "Predicted 2023 95% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 95% (QR)", "Predicted 1973 (EQ)" = "Predicted 1973 95% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 95% (QR)")

# Tables for visualizing the QR models
joined_qr_1 <- joined %>% arrange(`coefficient 1% (QR)`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 1%`, `P value 1%`, `coefficient 1% (QR)`, `Predicted 2023 1% (EQ)`, `Predicted 2023 1% (QR)`, `Predicted 1973 1% (EQ)`, `Predicted 1973 1% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 1%", 
         "P value (EQ)" = "P value 1%", "Coefficient (QR)" = "coefficient 1% (QR)", "Predicted 2023 (EQ)" = "Predicted 2023 1% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 1% (QR)", "Predicted 1973 (EQ)" = "Predicted 1973 1% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 1% (QR)")

joined_qr_50 <- joined %>% arrange(`coefficient 50% (QR)`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 50%`, `P value 50%`, `coefficient 50% (QR)`, `Predicted 2023 50% (EQ)`, `Predicted 2023 50% (QR)`, `Predicted 1973 50% (EQ)`, `Predicted 1973 50% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 50%", 
         "P value (EQ)" = "P value 50%", "Coefficient (QR)" = "coefficient 50% (QR)", "Predicted 2023 (EQ)" = "Predicted 2023 50% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 50% (QR)", "Predicted 1973 (EQ)" = "Predicted 1973 50% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 50% (QR)")

joined_qr_95 <- joined %>% arrange(`coefficient 95% (QR)`) %>% 
  dplyr::select(lat_name, station, latitude, `coefficient 95%`, `P value 95%`, `coefficient 95% (QR)`, `Predicted 2023 95% (EQ)`, `Predicted 2023 95% (QR)`, `Predicted 1973 95% (EQ)`, `Predicted 1973 95% (QR)`) %>%
  rename("Species" = lat_name, "Location" = station, "Latitude" = latitude, "Coefficient (EQ)" = "coefficient 95%", 
         "P value (EQ)" = "P value 95%", "Coefficient (QR)" = "coefficient 95% (QR)", "Predicted 2023 (EQ)" = "Predicted 2023 95% (EQ)", "Predicted 2023 (QR)" = "Predicted 2023 95% (QR)", "Predicted 1973 (EQ)" = "Predicted 1973 95% (EQ)", "Predicted 1973 (QR)" = "Predicted 1973 95% (QR)")




