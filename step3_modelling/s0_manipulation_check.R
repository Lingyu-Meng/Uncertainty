# Manipulation check
# This script is used to check the manipulation of the experiment' conditions without Kalman filter
# We will use a basic model (V) which will have slope and intercept for each condition
# Need to run s1_Kalman_filtering.m before running this script (if the data is not already available)

required_packages <- c("tidyverse","lme4","sjPlot","ggridges","car")

# Check and install missing packages
install_and_Load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE) # Load the required packages
  }
}

install_and_Load(required_packages)

# Load the data
data <- read_csv("step3_modelling/output/data.csv") %>% 
  mutate(selection = if_else(response == "Left", left, right),
         selection = case_when(
           grepl("R", selection) ~ "R",
           grepl("r", selection) ~ "r",
           grepl("S", selection) ~ "S",
           ),
         risky = if_else(selection == "R", 1, 0),
         condition = paste(context, arms), # combine it for convenience
         V = if_else(left == "**R**", V, -V)) # flip the value if the right arm is risky

# condition
dist_V <- data %>%
  ggplot(aes(x = V, y = arms, fill = condition)) +
  geom_density_ridges(alpha = 0.5) +
  theme_minimal() +
  facet_wrap(~context, ncol = 1) +
  labs(title = "Density plot of value difference by condition",
       x = "Value difference(risky - other)",
       y = "Density") +
  theme(legend.position = "top")

choice_proportion <- data %>%
  group_by(condition) %>%
  summarise(Proportion = mean(risky), context = unique(context)) %>%
  ggplot(aes(x = condition, y = Proportion, fill = context)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Proportion of risky choices by condition",
       x = "Condition",
       y = "Proportion of risky choices") +
  coord_flip()

# Modelling
condition_model <- glmer(risky ~ -1 + condition + condition:V + (-1 + V|ID),
                       data = data, family = binomial(link = "probit"),
                       control = glmerControl(optimizer = "bobyqa"))
summary(condition_model)

# Compare the intercepts for different conditions
post_hoc_inter <- lsmeans(condition_model, pairwise ~ condition | V, adjust = "none")

# Compare the slopes for different conditions
slope_comparison_1 <- linearHypothesis(
  condition_model, 
  "conditionLose rR:V = conditionLose SR:V")
slope_comparison_2 <- linearHypothesis(
  condition_model,
  "conditionWin rR:V = conditionWin SR:V")
slope_comparison_3 <- linearHypothesis(
  condition_model,
  "conditionLose rR:V = conditionWin rR:V")
slope_comparison_4 <- linearHypothesis(
  condition_model,
  "conditionLose SR:V = conditionWin SR:V")
slope_comparison <- rbind(slope_comparison_1, slope_comparison_2, slope_comparison_3, slope_comparison_4)

# Visualisation
risk_ratios <- plot_model(condition_model) +
  ylim(0.5, 2) 

slopes <- summary(condition_model)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "condition") %>% 
  filter(str_detect(condition, "V")) %>%
  mutate(condition = str_remove(condition, "condition"),
         condition = str_remove(condition, ":V")) %>%
  ggplot(aes(x = condition, y = Estimate,
             ymin = Estimate + `Std. Error` * -1.96,
             ymax = Estimate + `Std. Error` * 1.96)) +
  geom_pointrange() +
  geom_signif(comparisons = list(c("Lose rR", "Lose SR")),
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win rR", "Win SR")),
              annotation = c("***"), tip_length = 0) +
  theme_cowplot() +
  labs(x = "Condition",
       y = "Slope")

intercepts <- summary(condition_model)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "condition") %>% 
  filter(str_detect(condition, "V", negate = T)) %>%
  mutate(condition = str_remove(condition, "condition")) %>%
  ggplot(aes(x = condition, y = Estimate,
             ymin = Estimate + `Std. Error` * -1.96,
             ymax = Estimate + `Std. Error` * 1.96)) +
  geom_pointrange() +
  geom_signif(comparisons = list(c("Lose rR", "Lose SR")),
              y_position = 0,
              annotation = c("***"), tip_length = 0.03) +
  geom_signif(comparisons = list(c("Win rR", "Win SR")),
              annotation = c("***"), tip_length = 0.03) +
  geom_signif(comparisons = list(c("Lose rR", "Win rR")),
              y_position = -0.35, vjust = 3,
              annotation = c("***"), tip_length = -0.03) +
  geom_signif(comparisons = list(c("Lose SR", "Win SR")),
              y_position = -0.18, vjust = 3,
              annotation = c("***"), tip_length = -0.03) +
  theme_cowplot() +
  labs(x = "Condition",
       y = "Intercept")

coefficients <- cowplot::plot_grid(intercepts, slopes, nrow = 1)

Psychometric_curve <- plot_model(condition_model, type = "pred", terms=c("V [all]", "condition")) +
  theme_minimal() +
  labs(title = "Psychometric curve for each condition",
       x = "Value difference(risky - other)",
       y = "P(choose risky arm)")

psychometric_curve_adjusted <- cowplot::plot_grid(
  '',Psychometric_curve,'', rel_widths = c(0.4, 1, 0.4),
  nrow = 1, labels = c('', 'A', ''))

coefficients_labeled <- cowplot::plot_grid(
  intercepts, slopes, nrow = 1, labels = c('B', 'C'))

Conditions_Results <- cowplot::plot_grid(
  psychometric_curve_adjusted, coefficients_labeled, nrow = 2)

# Save
ggsave("step3_modelling/output/conditions_coefficients.png", coefficients, width = 10, height = 5)
ggsave("step3_modelling/output/conditions_risk_ratios.png", risk_ratios, width = 5, height = 5)
ggsave("step3_modelling/output/conditions_psychometric_curve.png", Psychometric_curve, width = 5, height = 5)
ggsave("step3_modelling/output/conditions_dist_V.png", dist_V, width = 5, height = 5)
ggsave("step3_modelling/output/conditions_choice_proportion.png", choice_proportion, width = 5, height = 5)
ggsave("step3_modelling/output/conditions_results.png", Conditions_Results, width = 10, height = 10)
ggsave("fig/fig2_panelB.png", intercepts, width = 5, height = 5)
ggsave("fig/fig2_panelC.png", slopes, width = 5, height = 5)
saveRDS(intercepts, "fig/fig2_panelB.rds")
saveRDS(slopes, "fig/fig2_panelC.rds")