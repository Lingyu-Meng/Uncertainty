# exploratory analysis of the data

# List of required packages
required_packages <- c("tidyverse",
                       "ggrain", # geom_rain
                       "cowplot", # theme_cowplot
                       "ggsignif", # geom_signif
                       "bruceR", # HLM_summary
                       "lme4")   # lmer

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

# Whether the first 6 trials are significantly different from the last 6 trials?
data <- read_csv("step1_data_wrangling/output/cleaned_data.csv") %>% 
  mutate(
    context = case_when(
      grepl("Win", `Spreadsheet: Condition`) ~ "Win",
      grepl("Lose", `Spreadsheet: Condition`) ~ "Lose",
    ),
    arms = case_when(
      grepl("S", `Spreadsheet: Condition`) ~ "SR",
      grepl("r", `Spreadsheet: Condition`) ~ "rR",
    )
  ) %>% 
  mutate(context = factor(context, levels = c("Win", "Lose")),
         arms = factor(arms, levels = c("SR", "rR")),
         lnRT = log(`Reaction Time`),
         trial = (`Trial Number` - 1) %% 12 + 1,
         trial_group = ifelse(trial <= 6, "First 6", "Last 6"),
         trial_group = factor(trial_group, levels = c("First 6", "Last 6"))
         )

# Performance
acc_data <- data %>% 
  group_by(trial_group, `Participant Private ID`) %>% # individual level
  summarise(
    Performance = sum(`Store: trial_correct`)/120,
  ) %>% 
  ungroup()
t.test(Performance ~ trial_group, data = acc_data)

acc_plot <- acc_data %>% 
  ggplot(aes(x = trial_group, y = Performance)) +
  geom_boxplot() +
  geom_signif(comparisons = list(c("First 6", "Last 6")), map_signif_level = TRUE) +
  theme_cowplot() 

# RT
rt_data <- data %>% 
  group_by(trial_group, `Participant Private ID`) %>% # individual level
  summarise(
    RT = mean(`Reaction Time`)
  ) %>% 
  ungroup() %>% 
  mutate(lnRT = log(RT))

t.test(lnRT ~ trial_group, data = rt_data)

rt_plot <- rt_data %>% 
  ggplot(aes(x = trial_group, y = RT)) +
  geom_boxplot() +
  geom_signif(comparisons = list(c("First 6", "Last 6")), map_signif_level = TRUE) +
  theme_cowplot()

trial_effect <- plot_grid(acc_plot, rt_plot, nrow = 1)

# Whether the model free analysis result remains for the first 6 trials?
# Accuracy
data_f6 <- data %>% 
  filter(trial <= 6)

acc_glmm_f6 <- glmer(`Store: trial_correct` ~  context * arms + (1|`Participant Private ID`),
    data = data_f6, family = "binomial")
summary(acc_glmm_f6)
acc_posthoc_f6 <- lsmeans(acc_glmm_f6, pairwise ~ context*arms, adjust = "tukey") # post hoc test

# Visiualise the HLM results
# Create a data context for visualization
relogit <- function(x) {
  exp(x)/(1+exp(x)) # reverse logit
}
vis_data_acc_f6 <- acc_posthoc_f6$lsmeans %>% 
  as.data.frame() %>%
  mutate(performance = relogit(lsmean),
         performance_LCI = relogit(asymp.LCL),
         performance_UCI = relogit(asymp.UCL)) 

# Plot the interaction
acc_cond_inter_f6 <- vis_data_acc_f6 %>% 
  ggplot(aes(x = context, y = performance, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = performance_LCI, ymax = performance_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0,
              y_position = 0.57) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("*"), tip_length = 0,
              y_position = 0.568, colour = "#00BFC4",
              vjust = 2.3) +
  geom_signif(y_position = 0.566, xmin = 0.95, xmax = 1.05, 
              annotation = c("***"), tip_length = 0, 
              colour = "black") +
  geom_signif(y_position = 0.566, xmin = 1.95, xmax = 2.05, 
              annotation = c("**"), tip_length = 0, 
              colour = "black") +
  labs(color = "Arms",
       y = "Performance ",
       x = "Context"
  ) +
  theme_cowplot()

# RT
glmm_RT_f6 <- glmer(`Reaction Time` ~ context*arms + (1|`Participant Private ID`),
                    data = data_f6, family = Gamma(link = "log")) # Gamma distribution
summary(glmm_RT_f6) # WIN SR as baseline
lnRT_posthoc_f6 <- lsmeans(glmm_RT_f6, pairwise ~ context:arms, adjust = "tukey") # post hoc test

# Visiualise the HLM results
vis_data_lnRT_f6 <- lnRT_posthoc_f6$lsmeans %>% 
  as.data.frame() %>%
  mutate(lnRT = lsmean,
         lnRT_LCI = asymp.LCL,
         lnRT_UCI = asymp.UCL)

# Plot the interaction
lnRT_cond_inter_f6 <- vis_data_lnRT_f6 %>% 
  ggplot(aes(x = context, y = lnRT, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = lnRT_LCI, ymax = lnRT_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              y_position = 6.745,
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("**"), tip_length = 0,
              y_position = 6.743,
              vjust = 2.3, colour = "#00BFC4") +
  geom_signif(y_position = 6.74, xmin = 0.95, xmax = 1.05,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 6.74, xmin = 1.95, xmax = 2.05,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  labs(color = "Arms",
       y = "Log RT",
       x = "Context") +
  theme_cowplot()

first_6 <- plot_grid(acc_cond_inter_f6, lnRT_cond_inter_f6, nrow = 1)

# Whether the model free analysis result remains for the last 6 trials?
# Accuracy
data_l6 <- data %>% 
  filter(trial > 6)

acc_glmm_l6 <- glmer(`Store: trial_correct` ~  context * arms + (1|`Participant Private ID`),
    data = data_l6, family = "binomial")
summary(acc_glmm_l6)
acc_posthoc_l6 <- lsmeans(acc_glmm_l6, pairwise ~ context*arms, adjust = "tukey") # post hoc test

# Visiualise the HLM results
# Create a data context for visualization
vis_data_acc_l6 <- acc_posthoc_l6$lsmeans %>% 
  as.data.frame() %>%
  mutate(performance = relogit(lsmean),
         performance_LCI = relogit(asymp.LCL),
         performance_UCI = relogit(asymp.UCL))

# Plot the interaction
acc_cond_inter_l6 <- vis_data_acc_l6 %>% 
  ggplot(aes(x = context, y = performance, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = performance_LCI, ymax = performance_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0,
              y_position = 0.665) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0,
              y_position = 0.663, colour = "#00BFC4",
              vjust = 2.3) +
  geom_signif(y_position = 0.66, xmin = 0.95, xmax = 1.05, 
              annotation = c("***"), tip_length = 0, 
              colour = "black") +
  geom_signif(y_position = 0.66, xmin = 1.95, xmax = 2.05, 
              annotation = c("***"), tip_length = 0, 
              colour = "black") +
  labs(color = "Arms",
       y = "Performance ",
       x = "Context"
  ) +
  theme_cowplot()

# RT
glmm_RT_l6 <- glmer(`Reaction Time` ~ context*arms + (1|`Participant Private ID`),
                    data = data_l6, family = Gamma(link = "log")) # Gamma distribution
summary(glmm_RT_l6) # WIN SR as baseline
lnRT_posthoc_l6 <- lsmeans(glmm_RT_l6, pairwise ~ context:arms, adjust = "tukey") # post hoc test

# Visiualise the HLM results
vis_data_lnRT_l6 <- lnRT_posthoc_l6$lsmeans %>% 
  as.data.frame() %>%
  mutate(lnRT = lsmean,
         lnRT_LCI = asymp.LCL,
         lnRT_UCI = asymp.UCL)

# Plot the interaction
lnRT_cond_inter_l6 <- vis_data_lnRT_l6 %>% 
  ggplot(aes(x = context, y = lnRT, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = lnRT_LCI, ymax = lnRT_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              y_position = 6.57,
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("**"), tip_length = 0,
              y_position = 6.567,
              vjust = 2.3, colour = "#00BFC4") +
  geom_signif(y_position = 6.56, xmin = 0.95, xmax = 1.05,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 6.56, xmin = 1.95, xmax = 2.05,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  labs(color = "Arms",
       y = "Log RT",
       x = "Context") +
  theme_cowplot()

last_6 <- plot_grid(acc_cond_inter_l6, lnRT_cond_inter_l6, nrow = 1)

# Save the plot
ggsave("step4_robust_check/check3_learning_effect/output/trial_effect.png", plot = trial_effect, width = 10, height = 5)
ggsave("step4_robust_check/check3_learning_effect/output/first_6.png", plot = first_6, width = 10, height = 5)
ggsave("step4_robust_check/check3_learning_effect/output/last_6.png", plot = last_6, width = 10, height = 5)