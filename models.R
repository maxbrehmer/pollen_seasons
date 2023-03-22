# Model: Quantile regression
# qr_10 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(lm(formula = greg_day ~ year, data = data)))
#qr_50 <- df %>% nest_by(lat_name, station) %>% rq(greg_day ~ year, tau = 0.5)
#qr_90 <- df %>% nest_by(lat_name, station) %>% rq(greg_day ~ year, tau = 0.9)

# Model: Linear regression on quantiles
#data_q10 <- df %>%
#  group_by(year) %>%
#  summarise(q10 = quantile(greg_day, prob = .1))

#data_q50 <- df %>%
#  group_by(year) %>%
#  summarise(q50 = quantile(greg_day, prob = .5))

#data_q90 <- df %>%
#  group_by(year) %>%
#  summarise(q90 = quantile(greg_day, prob = .9))

#eq_10 <- data_q10 %>% nest_by(lat_name, station) %>% lm(q10 ~ year)
#eq_50 <- data_q50 %>% nest_by(lat_name, station) %>% lm(q50 ~ year)
#eq_90 <- data_q90 %>% nest_by(lat_name, station) %>% lm(q90 ~ year)


#QR_5_sthlm <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.05)
#QR_50_sthlm <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.5)
#QR_95_sthlm <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.95)

#nonpar_qr <- function(area, genus, quantile) {
#  for (k in quantile) {
#    for (j in genus) {
#      for (i in area) {
#        qr <- rq(greg_day ~ year, data = i %>% filter(lat_name == j), tau = k)
#        summary.rq(qr)
#      }
#    }
#  }
#}

#area <- list(umea, eskil, sthlm, norrkp, jonkp, vastvik, gbg, malmo)
#genus <- c("Alnus", "Betula", "Corylus", "Poaceae", "Quercus", "Salix", "Ulmus")
#quantile <- c(0.05, 0.5, 0.95)

#get_qr <- nonpar_qr(area = area, genus = genus, quantile = quantile)

# Summary table
#srq_0.1 <- summary.rq(QR_0.1)
#srq_0.5 <- summary.rq(QR_0.5)
#srq_0.9 <- summary.rq(QR_0.9)

# Plots
#plot_qr <- function(area, genus, quantile) {
#  for (k in quantile) {
#    for (j in genus) {
#      for (i in area) {
#        plt <- plot(greg_day ~ year, data =  area[i] %>% filter(lat_name == genus[j]), tau = quantile[k])
#        mv(from = "plt", to = paste("plt", as.character(area[i]), as.character(genus[j]), as.character(quantile[k]), collapse = "_"))
#      }
#    }
#  }
#}

#get_qr_plot <- plot_qr(area = area, genus = genus, quantile = quantile)

#plot(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), pch = 16)
#abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.1), col = "green", lty = 1)
#abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.5), col = "red", lty = 1)
#abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.9), col = "blue", lty = 1)


