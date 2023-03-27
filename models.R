# Model: Quantile regression
qr_10 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.1))) %>% dplyr::select(c(-data))

# Model: Linear regression on quantiles
data_q10 <- df %>%
  group_by(year, lat_name, station) %>%
  summarise(q10 = quantile(greg_day, prob = .1))

eq_10 <- data_q10 %>% ungroup() %>% nest_by(lat_name, station) %>% mutate(model = list(lm(formula = q10 ~ year, data = data))) %>%
  dplyr::select(c(-data)) %>%
  mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

