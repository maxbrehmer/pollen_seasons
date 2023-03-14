# Model: Quantile Regression
QR_5_sthlm <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.05)
QR_50_sthlm <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.5)
QR_95_sthlm <- rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.95)

nonpar_qr <- function(area, genus, quantile) {
  for (k in quantile) {
    for (j in genus) {
      for (i in area) {
        qr <- rq(greg_day ~ year, data = area[i] %>% filter(lat_name = genus[j]), tau = quantile[k])
        mv(from = "qr", to = paste("qr", as.character(area[i]), as.character(genus[j]), as.character(quantile[k]), collapse = "_"))
      }
    }
  }
}

area <- c("umea", "eskil", "sthlm", "norrkp", "jonkp", "vastvik", "gbg", "malmo")
genus <- c("Alnus", "Betula", "Corylus", "Poaceae", "Quercus", "Salix", "Ulmus")
quantile <- c(0.05, 0.5, 0.95)

get_qr <- nonpar_qr(area = area, genus = genus, quantile = quantile)

# Summary table
srq_0.1 <- summary.rq(QR_0.1)
srq_0.5 <- summary.rq(QR_0.5)
srq_0.9 <- summary.rq(QR_0.9)

# Plots
plot_qr <- function(area, genus, quantile) {
  for (k in quantile) {
    for (j in genus) {
      for (i in area) {
        plt <- plot(greg_day ~ year, data =  area[i] %>% filter(lat_name = genus[j]), tau = quantile[k])
        mv(from = "plt", to = paste("plt", as.character(area[i]), as.character(genus[j]), as.character(quantile[k]), collapse = "_"))
      }
    }
  }
}

plot(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), pch = 16)
abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.1), col = "green", lty = 1)
abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.5), col = "red", lty = 1)
abline(rq(greg_day ~ year, data = sthlm %>% filter(lat_name == "Alnus"), tau = 0.9), col = "blue", lty = 1)