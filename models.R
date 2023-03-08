# Model: Quantile Regression
Quan_fit <- rq(greg_day ~ year, data = malmo %>% filter(lat_name == "Alder"))

summary(Quan_fit)