# Model: Quantile regression
#qr_1 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.01))) %>% 
#  dplyr::select(c(-data)) #%>%
  #mutate(coeff = summary.rq(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

#qr_50 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.5))) %>% 
#  dplyr::select(c(-data)) #%>%
  #mutate(coeff = summary.rq(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

#qr_95 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.95))) %>%
#  dplyr::select(c(-data)) #%>%
  #mutate(coeff = summary.rq(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

# Model: Linear regression on quantiles
data_q1 <- df %>%
  group_by(year, lat_name, station, latitude) %>%
  summarise(q1 = quantile(greg_day, prob = .01))

eq_1 <- data_q1 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>% mutate(model = list(lm(formula = q1 ~ year, data = data))) %>%
  dplyr::select(c(-data)) %>%
  mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

data_q50 <- df %>%
  group_by(year, lat_name, station, latitude) %>%
  summarise(q50 = quantile(greg_day, prob = .5))

eq_50 <- data_q50 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>% mutate(model = list(lm(formula = q50 ~ year, data = data))) %>%
  dplyr::select(c(-data)) %>%
  mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

data_q95 <- df %>%
  group_by(year, lat_name, station, latitude) %>%
  summarise(q95 = quantile(greg_day, prob = .95))

eq_95 <- data_q95 %>% ungroup() %>% nest_by(lat_name, station, latitude) %>% mutate(model = list(lm(formula = q95 ~ year, data = data))) %>%
  dplyr::select(c(-data)) %>%
  mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

eq_joined <- eq_1 %>%
  dplyr::select(lat_name, station, latitude, coeff, `P value`) %>%
  rename("coefficient 1%" = coeff, "P value 1%" = `P value`) %>%
  left_join(eq_50 %>% rename("coefficient 50%" = coeff, "P value 50%" = `P value`) %>% dplyr::select(lat_name, station, latitude, "coefficient 50%", "P value 50%"),
            by = c("lat_name", "station", "latitude")) %>%
  left_join(eq_95 %>% rename("coefficient 95%" = coeff, "P value 95%" = `P value`) %>% dplyr::select(lat_name, station, latitude, "coefficient 95%", "P value 95%"),
            by = c("lat_name", "station", "latitude"))



