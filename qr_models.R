# Model: Quantile regression
#qr_1 <- df %>% nest_by(station, lat_name) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.01))) %>% 
#dplyr::select(c(-data)) #%>%
#mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

#qr_50 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.5))) %>% 
#dplyr::select(c(-data)) #%>%
#mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)

#qr_95 <- df %>% nest_by(lat_name, station) %>% mutate(model = list(rq(formula = greg_day ~ year, data = data, tau = 0.95))) %>%
#dplyr::select(c(-data)) #%>%
#mutate(coeff = summary(model)$coefficients["year", "Estimate"], "P value" = summary(model)$coefficients["year", "Pr(>|t|)"], "Adj. R-squared" = summary(model)$adj.r.squared)
