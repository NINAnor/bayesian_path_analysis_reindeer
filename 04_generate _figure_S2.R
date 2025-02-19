load("mod_cent.RData")

# Get estimates and format the dataframe
est_mod_cent <- summary(mod_cent)$fixed
colnames(est_mod_cent) <- c("Estimate", "Est.Error", "lower_CI", "upper_CI", "Rhat", "Bulk_ESS", "Tail_ESS")
est_mod_cent <- round(est_mod_cent, digits = 5)

# Extract and format spring estimates
est_mod_cent_spring_intercept <- est_mod_cent[1, ] %>% select(-Est.Error)
est_mod_cent_spring_age <- est_mod_cent[7:19, ] %>% select(-Est.Error)
est_mod_cent_spring_age <- rbind(est_mod_cent_spring_intercept, est_mod_cent_spring_age)

# Extract and format success estimates
est_mod_cent_success_intercept <- est_mod_cent[2, ] %>% select(-Est.Error)
est_mod_cent_success_age <- est_mod_cent[25:37, ] %>% select(-Est.Error)
est_mod_cent_success_age <- rbind(est_mod_cent_success_intercept, est_mod_cent_success_age)
est_mod_cent_success_age <- est_mod_cent_success_age %>% 
  mutate(Estimate = plogis(as.numeric(Estimate)),
         lower_CI = plogis(as.numeric(lower_CI)),
         upper_CI = plogis(as.numeric(upper_CI)))

# Extract and format fall estimates
est_mod_cent_fall_intercept <- est_mod_cent[3, ] %>% select(-Est.Error)
est_mod_cent_fall_age <- est_mod_cent[45:57, ]  %>% select(-Est.Error)
est_mod_cent_fall_age <- rbind(est_mod_cent_fall_intercept, est_mod_cent_fall_age)

# Get age structure information
countage <- data %>% count(age)
spring_age_cent <- cbind(est_mod_cent_spring_age, countage)
success_age_cent <- cbind(est_mod_cent_success_age, countage)
fall_age_cent <- cbind(est_mod_cent_fall_age, countage)

# Plot success age
success_age_plot <- success_age_std %>% 
  ggplot() +
  geom_pointrange(aes(ymin = lower_CI, ymax = upper_CI, y = Estimate, x = age), color = "deepskyblue4") +
  geom_text(aes(label = paste("n =", n), x = age, y = upper_CI + 0.1), size = 3) +
  labs(y = bquote(Reproductive~success[t])) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 2, size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
print(success_age_plot)

# Extract the intercept values for spring
intercept_spring <- spring_age_cent[1, ]

# Add the intercept to the age-related estimates for spring
spring_age_cent <- spring_age_cent[-1, ] %>% 
  mutate(Estimate = Estimate + intercept_spring$Estimate,
         lower_CI = lower_CI + intercept_spring$lower_CI,
         upper_CI = upper_CI + intercept_spring$upper_CI)

# Add intercept as age = 2 for spring plot with n=116
spring_age_cent <- spring_age_cent %>% 
  add_row(age = 2, Estimate = intercept_spring$Estimate, lower_CI = intercept_spring$lower_CI, upper_CI = intercept_spring$upper_CI, n = 116)

# Plot spring age with intercept as age=2
spring_age_plot <- spring_age_cent %>% 
  ggplot() +
  geom_pointrange(aes(ymin = lower_CI, ymax = upper_CI, y = Estimate, x = age), color = "deepskyblue4") +
  geom_text(aes(label = paste("n =", n), x = age, y = upper_CI + 3), size = 3) +
  labs(y = bquote(Spring~body~mass[t]~(kg)),
       x = bquote(Age[t])) +
  scale_x_continuous(breaks = unique(spring_age_cent$age)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 2, size = 10),
        axis.text.y = element_text(size = 10))

# Print the plot
print(spring_age_plot)

# Extract the intercept values for fall
intercept_fall <- fall_age_cent[1, ]

# Add the intercept to the age-related estimates for fall
fall_age_cent <- fall_age_cent[-1, ] %>% 
  mutate(Estimate = Estimate + intercept_fall$Estimate,
         lower_CI = lower_CI + intercept_fall$lower_CI,
         upper_CI = upper_CI + intercept_fall$upper_CI)

# Add intercept as age = 2 for fall plot with n=116
fall_age_cent <- fall_age_cent %>% 
  add_row(age = 2, Estimate = intercept_fall$Estimate, lower_CI = intercept_fall$lower_CI, upper_CI = intercept_fall$upper_CI, n = 116)

# Plot fall age with intercept as age=2 and every step on x-axis
fall_age_plot <- fall_age_cent %>% 
  ggplot() +
  geom_pointrange(aes(ymin = lower_CI, ymax = upper_CI, y = Estimate, x = age), color = "deepskyblue4") +
  geom_text(aes(label = paste("n =", n), x = age, y = upper_CI + 3), size = 3) +
  labs(y = bquote(Fall~body~mass[t]~(kg)),
       x = bquote(Age[t])) +
  scale_x_continuous(breaks = unique(fall_age_cent$age)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 2, size = 10),
        axis.text.y = element_text(size = 10))

# Print the plot
print(fall_age_plot)
