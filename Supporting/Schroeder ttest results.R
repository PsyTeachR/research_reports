library(tidyverse)
library(janitor)

data <- read_csv("book/Supporting/Schroeder_hiring.csv") %>%
  clean_names(case = "snake") %>%
  mutate(condition = factor(condition, levels = c(0, 1), labels = c("Transcript", "Audio")))

(summary_stats <- data %>%
  group_by(condition) %>%
  summarise(mean = mean(intellect_rating),
            SD = sd(intellect_rating)))

broom::tidy(t.test(formula = intellect_rating ~ condition, data = data))

(cohens_d <- effectsize::cohens_d(intellect_rating ~ condition,
         pooled_sd = FALSE,
         data = data))

wilcox.test(formula = intellect_rating ~ condition, data = data, conf.int = TRUE)

# Good figure
data %>%
  ggplot(aes(x = condition, y = intellect_rating, fill = condition)) +
  geom_violin(trim = TRUE,
              show.legend = FALSE,
              alpha = .4) +
  geom_boxplot(width = .2,
               show.legend = FALSE,
               alpha = .7)+
  scale_x_discrete(name = "Experimental Group") +
  scale_y_continuous(name = "Mean Candidate Intellect Rating",
                     limits = c(0,10),
                     breaks = seq(0,10,2))+
  theme_minimal() +
  scale_fill_viridis_d()

ggsave("book/images/08_Schroeder_violin.png", bg = "white")

# Bad figure
data %>%
  ggplot(aes(x = condition, y = intellect_rating, fill = condition)) +
  geom_boxplot(width = .2,
               show.legend = FALSE,
               alpha = .7)+
  geom_violin(trim = TRUE,
              show.legend = FALSE,
              alpha = .4) +
  geom_point() +
  scale_x_discrete(name = "", labels = c("Group 1", "Group 2")) +
  scale_y_continuous(name = "Rating") +
  theme(legend.position = "none")

ggsave("book/images/08_Schroeder_violin_bad.png", bg = "white")

# Bad bar figure
data %>%
  ggplot(aes(x = condition, y = intellect_rating, fill = condition)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_x_discrete(name = "Experimental Group") +
  scale_y_continuous(name = "Mean Candidate Intellect Rating",
                     limits = c(0,10),
                     breaks = seq(0,10,2))+
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position = "none")

ggsave("book/images/08_Schroeder_violin_badbar.png", bg = "white")

