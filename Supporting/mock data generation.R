library(tidyverse)
library(faux)

# Week 7 Correlations

set.seed(1234)

Wingen_data <- rnorm_multi(
  n = 80,
  mu = c(Trust = 5, Replicability = 40),
  sd = c(Trust = 1.2, Replicability = 20),
  r = .25
)

# Tidy data to look more realistic
# Round values
Wingen_data$Trust <- round(Wingen_data$Trust, 2)
Wingen_data$Replicability <- round(Wingen_data$Replicability, 0)
# Create limits
Wingen_data$Trust[Wingen_data$Trust < 1] <- 1
Wingen_data$Trust[Wingen_data$Trust > 7] <- 7
Wingen_data$Replicability[Wingen_data$Replicability < 0] <- 0
Wingen_data$Replicability[Wingen_data$Replicability > 100] <- 100

cor.test(Wingen_data$Trust, Wingen_data$Replicability, alternative = "greater")

Wingen_data %>%
  summarise(across(.cols = Trust:Replicability, list(mean = mean, sd = sd)))

# Better example
Wingen_data %>%
  ggplot(aes(x = Trust, y = Replicability)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Trust in Psycholoy", y = "Replicability Estimate (%)") +
  scale_x_continuous(limits = c(1,7), breaks = seq(1,7,1)) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
  theme_minimal()

ggsave("book/Supporting/07_01.png", bg = "white")

# Weak example with code names and short limits
Wingen_data %>%
  ggplot(aes(x = Trust, y = Replicability)) +
  geom_point() +
  labs(x = "trust_01", y = "rep_est") +
  theme_minimal()

ggsave("book/Supporting/07_02.png", bg = "white")
