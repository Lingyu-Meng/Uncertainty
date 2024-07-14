# Describetive statistics
# input: cleaned_data.csv, inclusion.csv, questionnaire_score.csv, data_exp_172913-v29_questionnaire-y2w6.csv
# output: age_hist.png, gender_bar, age_gender_rain.png, RT_hist.png, trait_corr.png, rain_meanRT_context_arms.png, RT_cond_inter.png, response_rate_stack.png, acc_hist.png, accuracy_rain.png, acc_cond_inter.png, RT_acc.png, acc_trait_RT_plot.png

# List of required packages
required_packages <- c("tidyverse",
                       "ggrain", # geom_rain
                       "cowplot", # theme_cowplot
                       "ggsignif", # geom_signif
                       "bruceR", # HLM_summary
                       "lme4") # lmer

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

## Demographic statistics
# Load the demographic data
demographic_data <- read_csv("raw_data/data_exp_172913-v29/data_exp_172913-v29_questionnaire-y2w6.csv") %>% 
  head(-1) # remove the last row

# read excluded participants
excluded_participants <- read_csv("step1_data_wrangling/output/inclusion.csv") %>% 
  filter(pass == FALSE)

demo <- demographic_data %>% 
  filter(Key == "value") %>% 
  select(`Participant Private ID`, Question, Response) %>% 
  mutate(Question = sub(".*\\b(\\w+)\\W*$", "\\1", Question)) %>%  # keep the last word in the question
  spread(Question, Response) %>% 
  mutate(age = as.numeric(age)) %>%
  anti_join(excluded_participants, by = join_by(`Participant Private ID`)) # remove excluded participants

# age
age_hist <- demo %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Frequency") +
  theme_cowplot()

# gender
male_female_prop <- demo %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, n) %>% 
  mutate(sum = sum(Female, Male))

# male female proportion test
MFP_test <- prop.test(male_female_prop$Female, male_female_prop$sum)
# X-squared = 9.0619, df = 1, p-value = 0.00261

gender_bar <- demo %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = gender, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Gender Distribution",
       y = "Frequency") +
  geom_signif(comparisons = list(c("Female", "Male")),
              annotations = "**") +
  theme_cowplot()

# age x gender
age_gender_rain <- demo %>% 
  ggplot(aes(x = gender, y = age, fill = gender)) +
  geom_rain(rain.side = 'l') +
  theme_cowplot()

# male famel age test
ttest_gender_age <- demo %>% 
  filter(gender == "Female" | gender == "Male") %>%
  mutate(gender = as.factor(gender)) %>% 
  t.test(age ~ gender,.) # The degrees of freedom associated with this variance estimate is approximated using the Welch–Satterthwaite equation
# t = 0.70847, df = 85.374, p-value = 0.4806

age_2gender_rain <- demo %>% 
  filter(gender == "Female" | gender == "Male") %>%
  ggplot(aes(x = gender, y = age, fill = gender)) +
  geom_rain(rain.side = 'l') +
  geom_signif(comparisons = list(c("Female", "Male")),
              annotations = "NS.") +
  theme_cowplot()

## Questionnaire
# Load the questionnaire data
questionnaire_data <- read_csv("step1_data_wrangling/output/questionnaire_score.csv") %>% 
  transmute(
    `Participant Private ID` = `Participant Private ID`,
    IU = IU,
    IUp = PIU,
    IUi = IIU,
    IM = `Overall impulsiveness`,
    Anxi = Overall_anxiety
  )

# correlation matrix
trait_cor <- questionnaire_data %>% 
  select(-`Participant Private ID`) %>%
  Corr()

## RT distribution
# Load the RT data
RT_data <- read_csv("step1_data_wrangling/output/cleaned_data.csv") %>% 
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
  select(`Participant Private ID`, `Reaction Time`, context, arms) %>% 
  left_join(demo, by = "Participant Private ID") %>% 
  mutate(context = factor(context, levels = c("Win", "Lose")),
         arms = factor(arms, levels = c("SR", "rR")),
         lgRT = log(`Reaction Time`))

# RT
RT_hist <- RT_data %>% 
  ggplot(aes(x = `Reaction Time`)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "RT Distribution",
       x = "RT (ms)",
       y = "Frequency") +
  theme_cowplot()
lgRT_hist <- RT_data %>% 
  ggplot(aes(x = lgRT)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Log RT Distribution",
       x = "Log RT",
       y = "Frequency") +
  theme_cowplot()
RT_dist <- plot_grid(RT_hist, lgRT_hist, ncol = 1)

# RT x Condition
# calculate the mean RT in same condition per participants
mean_RT <- RT_data %>% 
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(`Mean RT` = mean(`Reaction Time`),
            `Mean lgRT` = mean(lgRT)) %>%
  ungroup(`Participant Private ID`, context, arms)

rain_meanlgRT_context_arms <- mean_RT %>% 
  mutate(`Participant Private ID` = ifelse(
    arms == "SR", `Participant Private ID` * 10,
    `Participant Private ID`) # to separate the two conditions
  ) %>%
  ggplot(aes(x = context, y = `Mean lgRT`, fill = arms)) +
  geom_rain(rain.side = 'f2x2', id.long.var = "Participant Private ID", alpha = 0.5) +
  theme_cowplot()

rain_meanRT_context_arms <- mean_RT %>% 
  mutate(`Participant Private ID` = ifelse(
    arms == "SR", `Participant Private ID` * 10,
    `Participant Private ID`) # to separate the two conditions
  ) %>%
  ggplot(aes(x = context, y = `Mean RT`, fill = arms, color = arms)) +
  geom_rain(rain.side = 'f2x2', id.long.var = "Participant Private ID", alpha = 0.5) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              color = "black") +
  theme_cowplot()

# RT Linear Mixed Model (RT ~ context:arms + (1|Participant Private ID)
lmm_lgRT <- RT_data %>% 
  lmer(lgRT ~ context*arms + (1|`Participant Private ID`), data = .)
HLM_summary(lmm_lgRT) # WIN SR as baseline
lgRT_posthoc <- lsmeans(lmm_RT, pairwise ~ context:arms, adjust = "tukey") # post hoc test
# Only Win SR - Win rR and Lose SR - Lose rR is not significant
# Or conduct ANOVA, which may has less power, but the results are the same
# RT_aov <- RT_data %>%
#   aov(`Reaction Time` ~ context*arms + Error(`Participant Private ID`), data = .)
# summary(RT_aov) # main effect of context only

# This result is consistent with the analysis without log trans
lmm_RT <- RT_data %>% 
  lmer(`Reaction Time` ~ context*arms + (1|`Participant Private ID`), data = .)
HLM_summary(lmm_RT) # WIN SR as baseline
RT_posthoc <- lsmeans(lmm_RT, pairwise ~ context:arms, adjust = "tukey") # post hoc test
# only difference is interaction. context x arms is p = 0.065 in log, whereas p = 0.152 in original RT

# Visiualise the HLM results
# Create a data context for visualization
vis_data <- expand.grid(context = levels(RT_data$context),
                        arms = levels(RT_data$arms))

vis_data$RT <-  predict(lmm_RT, newdata = vis_data, re.form = NA)
vis_data$lgRT <-  predict(lmm_lgRT, newdata = vis_data, re.form = NA)

# Plot the interaction
RT_cond_inter <- vis_data %>% 
  ggplot(aes(x = context, y = RT, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              y_position = 754,
              vjust = 2.3, colour = "#00BFC4") +
  geom_signif(y_position = 710, xmin = 0.99, xmax = 1.01,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 735, xmin = 1.99, xmax = 2.01,
              annotation = c("NS."), tip_length = 0,
              colour = "black", vjust = 2) +
  labs(title = "Interaction Plot for RT Results",
       color = "Arms",
       y = "Log RT",
       x = "Context") +
  theme_cowplot()

lgRT_cond_inter <- vis_data %>% 
  ggplot(aes(x = context, y = lgRT, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              y_position = 6.344,
              vjust = 2.3, colour = "#00BFC4") +
  geom_signif(y_position = 6.25, xmin = 0.99, xmax = 1.01,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 6.325, xmin = 1.99, xmax = 2.01,
              annotation = c("NS."), tip_length = 0,
              colour = "black", vjust = 2) +
  labs(title = "Interaction Plot for RT Results",
       color = "Arms",
       y = "Log RT",
       x = "Context") +
  theme_cowplot()

RTs_inter_plot <- plot_grid(RT_cond_inter, lgRT_cond_inter, ncol = 1)

## Response rate
response_rate <- RT_data %>% 
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(`Response Rate` = n() / 60) %>%
  ungroup(`Participant Private ID`, context, arms) %>% 
  mutate(Grade = case_when(
    `Response Rate` == 1 ~ "100%",
    `Response Rate` > 0.9 ~ ">90%",
    `Response Rate` > 0.8 ~ ">80%",
    `Response Rate` > 0.6 ~ ">60%",
    TRUE ~ as.character(`Response Rate`)
  ))

response_rate_grade <- response_rate %>% 
  group_by(context, arms, Grade) %>%
  summarise(Count = n()) %>%
  group_by(context, arms) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup() %>% 
  unite("Condition", c("context", "arms"), sep = "_")

# response rate ~ context x arms
response_rate_stack <- response_rate_grade %>% 
  ggplot(aes(x = Condition, y = Proportion, fill = Grade)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1.05)) +
  labs(
    title = "Response Rates by Conditions",
    x = "Condition",
    y = "Proportion of Participants",
    fill = "Response Rate"
  ) +
  theme_cowplot() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  geom_signif(comparisons = list(c("Win_SR", "Lose_rR")),
              tip_length = 0, y_position = 1.01,
              annotation = c("NS."))

# chi-square test
response_rate_chi <- response_rate_grade %>% 
  xtabs(Count ~ Condition + Grade, data = .) %>%
  chisq.test()
# X-squared = 9.2843, df = 9, p-value = 0.4115

## Accuracy
# Load the accuracy data
accuracy_data <- read_csv("step1_data_wrangling/output/cleaned_data.csv") %>% 
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
  group_by(`Participant Private ID`, `Spreadsheet: block_flag`) %>%
  transmute(`Participant Private ID`, context, arms,
            performance = max(`Store: round_correct`, na.rm = T)/12) %>%
  unique() %>%
  left_join(demo, by = "Participant Private ID") %>% 
  mutate(context = factor(context, levels = c("Win", "Lose")),
         arms = factor(arms, levels = c("SR", "rR")))

acc_hist <- accuracy_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(performance = mean(performance)) %>%
  ggplot(aes(x = performance)) +
  geom_histogram(binwidth = 0.02, fill = "skyblue", color = "black") +
  labs(title = "Accuracy Distribution",
       x = "Accuracy",
       y = "Frequency") +
  theme_cowplot()

# accuracy ~ context x arms
acc_aov <- accuracy_data %>%
  aov(performance ~ context*arms + Error(`Participant Private ID`), data = .)
summary(acc_aov)

acc_lmm <- accuracy_data %>% 
  lmer(performance ~ context*arms + (1|`Participant Private ID`), data = .)
HLM_summary(acc_lmm) # Lose rR as baseline
acc_posthoc <- lsmeans(acc_lmm, pairwise ~ context*arms, adjust = "tukey") # post hoc test

accuracy_rain <- accuracy_data %>% 
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(performance = mean(performance)) %>%
  ggplot(aes(x = context, y = performance, fill = arms)) +
  geom_rain(rain.side = 'f2x2', id.long.var = "Participant Private ID", alpha = 0.5) +
  theme_cowplot()

# Visiualise the HLM results
# Create a data context for visualization
vis_data$performance <-  predict(acc_lmm, newdata = vis_data, re.form = NA)

# Plot the interaction
acc_cond_inter <- vis_data %>% 
  ggplot(aes(x = context, y = performance, color = arms, group = arms)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0,
              y_position = 0.5, colour = "#00BFC4") +
  geom_segment(aes(x = 0.9, xend = 0.9, y = 0.509, yend = 0.57),
               color = "black") +
  annotate("text", x = 0.85, y = 0.54, label = "p = 0.0156",
           vjust = 2.3, size = 5, angle = 90) +
  geom_segment(aes(x = 2.1, xend = 2.1, y = 0.532, yend = 0.578),
               color = "black") +
  annotate("text", x = 2.1, y = 0.56, label = "p = 0.1386",
           vjust = 2.3, size = 5, angle = 90) +
  labs(title = "Interaction Plot for Accuracy Results",
       color = "Arms",
       y = "Accuracy",
       x = "Context") +
  theme_cowplot()

## RT x Accuracy by context x arms
RT_acc <- vis_data %>% 
  ggplot(aes(x = RT, y = performance, color = arms,
             shape = context, group = arms)) +
  geom_line(size = 1) +
  geom_point(size = 4) +
  labs(title = "RT x Accuracy",
       x = "RT (ms)",
       y = "Accuracy") +
  theme_cowplot()

## trait x accuracy
accuracy_trait <- accuracy_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(performance = mean(performance)) %>%
  left_join(questionnaire_data, by = "Participant Private ID")

acc_IU <- accuracy_trait %>% 
  ggplot(aes(x = IU, y = performance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

acc_IM <- accuracy_trait %>% 
  ggplot(aes(x = IM, y = performance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

acc_anx <- accuracy_trait %>% 
  ggplot(aes(x = Anxi, y = performance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

acc_trait_plot <- plot_grid(acc_IU, acc_IM, acc_anx, ncol = 3)

acc_trait_lm <- accuracy_trait %>% 
  lm(performance ~ IU*IM*Anxi, data = .)
summary(acc_trait_lm)

## trait x RT
RT_trait <- RT_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(`Mean RT` = mean(`Reaction Time`),
            `Mean lgRT` = mean(lgRT)) %>%
  left_join(questionnaire_data, by = "Participant Private ID")

RT_IU <- RT_trait %>%
  ggplot(aes(x = IU, y = `Mean RT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

RT_IM <- RT_trait %>%
  ggplot(aes(x = IM, y = `Mean RT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

RT_anx <- RT_trait %>%
  ggplot(aes(x = Anxi, y = `Mean RT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

RT_trait_plot <- plot_grid(RT_IU, RT_IM, RT_anx, ncol = 3)

RT_trait_lm <- RT_trait %>% 
  lm(`Mean RT` ~ IU*IM*Anxi, data = .)
summary(RT_trait_lm)
# original RT is marginal significant in IU (p = 0.095) and IM (p = 0.098)
lgRT_trait_lm <- RT_trait %>% 
  lm(`Mean lgRT` ~ IU*IM*Anxi, data = .)
summary(lgRT_trait_lm)
# log RT is not significant in IU, IM, and Anxi

acc_trait_RT_plot <- plot_grid(acc_trait_plot, RT_trait_plot, ncol = 1)

## Save the plots
ggsave("step2_descriptive_statistics/output/age_hist.png", age_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/gender_bar.png", gender_bar, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/age_gender_rain.png", age_gender_rain, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/age_2gender_rain.png", age_2gender_rain, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_hist.png", RT_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/lgRT_hist.png", lgRT_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_dist.png", RT_dist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/trait_corr.png", trait_cor$plot, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/rain_meanRT_context_arms.png", rain_meanRT_context_arms, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_condition_interaction.png", RT_cond_inter, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RTs_inter_plot.png", RTs_inter_plot, width = 8, height = 12)
ggsave("step2_descriptive_statistics/output/response_rate_stack.png", response_rate_stack, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/accuracy_hist.png", acc_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/accuracy_rain.png", accuracy_rain, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/accuracy_condition_interaction.png", acc_cond_inter, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_accuracy.png", RT_acc, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/trait_accuracy_RT_plot.png", acc_trait_RT_plot, width = 10, height = 6)