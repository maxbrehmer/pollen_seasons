# Model: Quantile regression
qr_1 <- df %>% nest_by(station, lat_name) %>% mutate(model = list(rq(formula = greg_day ~ Year, data = data, tau = 0.01))) %>% 
  rowwise() %>%
  mutate(`Predicted 2050 1% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                          data = data, tau = 0.01), newdata = data.frame(Year = 77))),
         `Predicted 2023 1% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                          data = data, tau = 0.01), newdata = data.frame(Year = 50))), 
         `Predicted 1973 1% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                          data = data, tau = 0.01), newdata = data.frame(Year = 0)))) %>%
  dplyr::select(c(-data))


qr_50 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ Year, data = data, tau = 0.5))) %>% 
  rowwise() %>%
  mutate(`Predicted 2050 50% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                          data = data, tau = 0.5), newdata = data.frame(Year = 77))),
         `Predicted 2023 50% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                           data = data, tau = 0.5), newdata = data.frame(Year = 50))), 
         `Predicted 1973 50% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                           data = data, tau = 0.5), newdata = data.frame(Year = 0)))) %>%
  dplyr::select(c(-data))


qr_95 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ Year, data = data, tau = 0.95))) %>%
  rowwise() %>%
  mutate(`Predicted 2050 95% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                          data = data, tau = 0.95), newdata = data.frame(Year = 77))),
         `Predicted 2023 95% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                           data = data, tau = 0.95), newdata = data.frame(Year = 50))), 
         `Predicted 1973 95% (QR)` = as.integer(predict(rq(greg_day ~ Year, 
                                                           data = data, tau = 0.95), newdata = data.frame(Year = 0)))) %>%
  dplyr::select(c(-data))


