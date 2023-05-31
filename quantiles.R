# Setting years before 2023 as starting at 2023 and going backwards
df <- df %>% mutate(Year = year - 1973)

################################################################################
############################### (QR) DATA FRAME ################################
################################################################################

# QR performance testing
QR <- rq(formula = greg_day ~ Year, data = df %>% filter(station == "Stockholm", lat_name == "Betula"), tau = 1:99/100)

# Creating data frame and adding confidence interval
QR_tidy <- QR %>%
  broom::tidy(se.type = "rank", conf.int = TRUE, conf.level = 0.95) %>%
  filter(!grepl("factor", term))

################################################################################
############################### (LM) DATA FRAME ################################
################################################################################

# Creating linear OLS regression model
lm <- lm(data = df %>% filter(station == "Stockholm", lat_name == "Betula"), formula = greg_day ~ Year)

ols <- as.data.frame(coef(lm))
ols.ci <- as.data.frame(confint(lm))
ols2 <- cbind(ols, ols.ci)
ols2 <- tibble::rownames_to_column(ols2, var="term")

################################################################################
############################### (EQ) DATA FRAME ################################
################################################################################

# Generate a sequence of quantile levels from 0.01 to 0.99
quantile_levels <- seq(0.01, 0.99, by = 0.01)

# Create an empty data frame to store the coefficient estimates and confidence intervals
lm_quantile_df_tau <- data.frame(term = character(2*length(quantile_levels)), 
                                 estimate = numeric(2*length(quantile_levels)), 
                                 conf.low = numeric(2*length(quantile_levels)), 
                                 conf.high = numeric(2*length(quantile_levels)), 
                                 tau = numeric(2*length(quantile_levels)))

# Create an empty list to store the data frames
quantile_data_frames <- list()

# Loop through each quantile level and compute the quantile value
for (i in seq_along(quantile_levels)) {
  tau <- quantile_levels[i]
  
  # Compute the quantile for the current quantile level
  quantile_value <- df %>%
    filter(station == "Stockholm", lat_name == "Betula") %>%
    group_by(Year, lat_name, station, latitude) %>%
    summarise(!!paste0("q", i) := quantile(greg_day, prob = tau))
  
  # Store the quantile value in a separate data frame
  quantile_data_frames[[i]] <- data.frame(quantile_value)
  
  # Fit linear regression for the current quantile level
  lm_quantile <- lm(formula = quantile_data_frames[[i]][[5]] ~ Year, data = quantile_data_frames[[i]])
  
  # Extract coefficient estimates and confidence intervals
  lm_quantile_coef <- coef(lm_quantile)
  lm_quantile_ci <- confint(lm_quantile, level = 0.95)
  
  # Assign the results to the corresponding row in the data frame
  lm_quantile_df_tau[2*i-1, "term"] <- "(Intercept)"
  lm_quantile_df_tau[2*i-1, "estimate"] <- lm_quantile_coef[1]
  lm_quantile_df_tau[2*i-1, "conf.low"] <- lm_quantile_ci["(Intercept)", 1]
  lm_quantile_df_tau[2*i-1, "conf.high"] <- lm_quantile_ci["(Intercept)", 2]
  lm_quantile_df_tau[2*i-1, "tau"] <- tau
  
  lm_quantile_df_tau[2*i, "term"] <- "Year"
  lm_quantile_df_tau[2*i, "estimate"] <- lm_quantile_coef[2]
  lm_quantile_df_tau[2*i, "conf.low"] <- lm_quantile_ci["Year", 1]
  lm_quantile_df_tau[2*i, "conf.high"] <- lm_quantile_ci["Year", 2]
  lm_quantile_df_tau[2*i, "tau"] <- tau
}

################################################################################
############################ QUANTILE ESTIMATE PLOT ############################
################################################################################

# Define colors for QR and EQ plots
qr_color <- "#512ffa"  # QR plot color
eq_color <- "#56de47"  # EQ plot color

# Create the plot
plotQR <- QR_tidy %>% 
  ggplot(aes(x=tau,y=estimate)) +
  
  # QR
  geom_line(aes(color="QR"), linewidth = 0.5, show.legend = FALSE) + # Set color to "QR"
  geom_hline(data = ols2, aes(yintercept= `coef(lm)`), lty=1, color="#eb2a3d", linewidth=0.5) + 
  geom_hline(aes(yintercept = 0), lty=2, color="black", linewidth=0.5) + 
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill="QR"), alpha=0.25) + # Set fill to "QR"
  
  # EQ
  geom_line(data = lm_quantile_df_tau, aes(x = tau, y = estimate, group = term, color="EQ"), 
            linewidth = 0.5, show.legend = FALSE) + # Set color to "EQ"
  geom_ribbon(data = lm_quantile_df_tau, aes(x = tau, ymin = conf.low, ymax = conf.high, fill="EQ"), 
              alpha = 0.25) + # Set fill to "EQ"
  
  facet_wrap(~term, scales="free", ncol=2) +
  
  # Customize legend title and labels
  labs(color = "Method", fill = "Method") +
  scale_color_manual(values = c("QR" = qr_color, "EQ" = eq_color)) +
  scale_fill_manual(values = c("QR" = qr_color, "EQ" = eq_color))

################################################################################
##################### OVERLAYED REGRESSION LINE/POINT PLOT #####################
################################################################################

trend_data <- df %>% filter(station == "Stockholm", lat_name == "Betula")
trend_summarised <- df %>% group_by(Year, lat_name, station, latitude) %>%
  summarise(q1 = quantile(greg_day, prob = .01)) %>% filter(station == "Stockholm", lat_name == "Betula")

plot_trend <- ggplot() +
  geom_jitter(data = trend_data, aes(x = Year, y = greg_day)) +
  geom_quantile(data = trend_data, aes(x = Year, y = greg_day, color = "QR"), quantiles = c(0.01), size = 1) +
  geom_jitter(data = trend_summarised, aes(x = Year, y = q1, color = "EQ")) +
  geom_smooth(data = trend_summarised, method = "lm", aes(x = Year, y = q1, color = "EQ")) +
  labs(x = "Year", y = "Day") +
  scale_color_manual(name = "Method", 
                     values = c(QR = "#512ffa", EQ = "#56de47"), 
                     labels = c(QR = "QR", EQ = "EQ"))



