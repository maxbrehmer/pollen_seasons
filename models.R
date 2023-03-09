# Model: Quantile Regression
QR_0.1 <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.1)
QR_0.5 <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.5)
QR_0.9 <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.9)

# Summary table
srq_0.1 <- summary.rq(QR_0.1)
srq_0.5 <- summary.rq(QR_0.5)
srq_0.9 <- summary.rq(QR_0.9)

# Plots
plot(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), pch = 16)
abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.1), col = "green", lty = 1)
abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.5), col = "red", lty = 1)
abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.99), col = "blue", lty = 1)