# Describetive statistics
# input: cleaned_data.csv, inclusion.csv, questionnaire_score.csv, data_exp_172913-v29_questionnaire-y2w6.csv
# output: age_hist.png, gender_bar, age_gender_rain.png, RT_hist.png, trait_corr.png, rain_meanRT_context_arms.png, RT_cond_inter.png, response_rate_stack.png, acc_hist.png, accuracy_rain.png, acc_cond_inter.png, RT_acc.png, acc_trait_RT_plot.png

# List of required packages
required_packages <- c("tidyverse",
                       "ggrain", # geom_rain
                       "cowplot", # theme_cowplot
                       "ggsignif", # geom_signif
                       "bruceR", # HLM_summary
                       "lme4",   # lmer
                       "sjPlot") # plot_model

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
  geom_signif(comparisons = list(c("Female", "Male")),
              annotations = "NS.") +
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

## Traits
# Load the questionnaire data
traits_data <- read_csv("step1_data_wrangling/output/questionnaire_score.csv") %>% 
  transmute(
    `Participant Private ID` = `Participant Private ID`,
    IU = IU,
    IUp = PIU,
    IUi = IIU,
    IM = `Overall impulsiveness`,
    Anxi = Overall_anxiety
  )

# load risk aversion
risk_aversion <- read_csv("step2_descriptive_statistics/output/risk_aversion.csv") %>% 
  transmute(`Participant Private ID` = ParticipantPrivateID,
            RA = gamma) %>%  # Risk Aversion
  unique()

traits_data <- traits_data %>% 
  left_join(risk_aversion, by = "Participant Private ID")

# correlation matrix
trait_cor <- traits_data %>% 
  select(-`Participant Private ID`) %>%
  Corr(p.adjust = "bonferroni")

# Scatter plot
# Open a PNG device, as pairs cannot be save in ggsave way
png(filename = "step2_descriptive_statistics/output/traits_pairs_plot.png", width = 1000, height = 1000)

# Generate the pairs plot
trait_scatter <- traits_data %>% 
  select(-`Participant Private ID`) %>%
  pairs()

# Close the device
dev.off()

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
RT_dist <- cowplot::plot_grid(RT_hist, lgRT_hist, ncol = 1)

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

# RT Generalised Linear Mixed Model (RT ~ context:arms + (1|Participant Private ID)
glmm_RT <- RT_data %>% 
  glmer(`Reaction Time` ~ context*arms + (1|`Participant Private ID`),
        data = ., family = Gamma(link = "log")) # Gamma distribution
summary(glmm_RT) # WIN SR as baseline
lgRT_posthoc <- lsmeans(glmm_RT, pairwise ~ context:arms, adjust = "tukey") # post hoc test
# Only Win SR - Win rR and Lose SR - Lose rR is not significant
# Or conduct ANOVA, which may has less power, but the results are the same
# RT_aov <- RT_data %>%
#   aov(`Reaction Time` ~ context*arms + Error(`Participant Private ID`), data = .)
# summary(RT_aov) # main effect of context only


# Visiualise the HLM results
vis_data_RT <- RT_posthoc$lsmeans %>% 
  as.data.frame() %>%
  mutate(RT = lsmean,
         RT_LCI = asymp.LCL,
         RT_UCI = asymp.UCL)

vis_data_lgRT <- lgRT_posthoc$lsmeans %>% 
  as.data.frame() %>%
  mutate(lgRT = lsmean,
         lgRT_LCI = asymp.LCL,
         lgRT_UCI = asymp.UCL)

# Plot the interaction
RT_cond_inter <- vis_data_RT %>% 
  ggplot(aes(x = context, y = RT, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = RT_LCI, ymax = RT_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              y_position = 792,
              vjust = 2.3, colour = "#00BFC4") +
  labs(title = "Interaction Plot for RT Results",
       color = "Arms",
       y = "RT (ms)",
       x = "Context") +
  theme_cowplot()

lgRT_cond_inter <- vis_data_lgRT %>% 
  ggplot(aes(x = context, y = lgRT, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = lgRT_LCI, ymax = lgRT_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              y_position = 6.645,
              vjust = 2.3, colour = "#00BFC4") +
  labs(color = "Arms",
       y = "Log RT",
       x = "Context") +
  theme_cowplot()

RTs_inter_plot <- cowplot::plot_grid(RT_cond_inter, lgRT_cond_inter, ncol = 1)

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
correct_data <- read_csv("step1_data_wrangling/output/cleaned_data.csv") %>% # trial level
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
  transmute(`Participant Private ID`, context, arms,
            correct = `Store: trial_correct`)

accuracy_data <- correct_data%>% # condition level
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(Performance = mean(correct)) %>%
  left_join(demo, by = "Participant Private ID") %>% 
  mutate(context = factor(context, levels = c("Win", "Lose")),
         arms = factor(arms, levels = c("SR", "rR")))

acc_hist <- accuracy_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(performance = mean(Performance)) %>%
  ggplot(aes(x = performance)) +
  geom_histogram(binwidth = 0.02, fill = "skyblue", color = "black") +
  labs(title = "Accuracy Distribution",
       x = "Accuracy",
       y = "Frequency") +
  theme_cowplot()

# accuracy ~ context x arms
acc_glmm <- glmer(correct ~ context*arms + (1|`Participant Private ID`),
                  data = correct_data, family = "binomial") # as GLMM is singular
HLM_summary(acc_glmm) # Lose rR as baseline
acc_posthoc <- lsmeans(acc_glmm, pairwise ~ context*arms, adjust = "tukey") # post hoc test

accuracy_rain <- accuracy_data %>% 
  mutate(`Participant Private ID` = ifelse(
    arms == "SR", `Participant Private ID` * 10,
    `Participant Private ID`) # to separate the two conditions
  ) %>%
  ggplot(aes(x = context, y = Performance, fill = arms, colour = arms)) +
  geom_rain(rain.side = 'f2x2', id.long.var = "Participant Private ID", alpha = 0.5) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("*"), tip_length = 0,
              y_position = 0.785, colour = "#00BFC4",
              vjust = 2.3) +
  geom_signif(y_position = 0.81, xmin = 0.85, xmax = 0.95,
              annotation = c("***"), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 0.81, xmin = 2.05, xmax = 2.15,
              annotation = c("***"), tip_length = 0,
              colour = "black") +
  theme_cowplot()

# Visiualise the HLM results
# Create a data context for visualization
relogit <- function(x) {
  exp(x)/(1+exp(x)) # reverse logit
}
vis_data_acc <- acc_posthoc$lsmeans %>% 
  as.data.frame() %>%
  mutate(performance = relogit(lsmean),
         performance_LCI = relogit(asymp.LCL),
         performance_UCI = relogit(asymp.UCL)) %>% 
  mutate(context = factor(context, levels = c("Win", "Lose")), # reorder the levels
         arms = factor(arms, levels = c("SR", "rR")))

# Plot the interaction
acc_cond_inter <- vis_data_acc %>% 
  ggplot(aes(x = context, y = performance, color = arms, group = arms)) +
  geom_line(size = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = performance_LCI, ymax = performance_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("*"), tip_length = 0,
              y_position = 0.603, colour = "#00BFC4",
              vjust = 2.3) +
  labs(color = "Arms",
       y = "Accuracy",
       x = "Context"
       ) +
  theme_cowplot()

## RT x Accuracy by context x arms
vis_data <- vis_data_acc %>% 
  transmute(context = context, arms = arms,
            Performance = performance,
            performance_LCI = performance_LCI,
            performance_UCI = performance_UCI) %>%
  left_join(vis_data_RT, by = c("context", "arms"))

RT_acc <- vis_data %>% 
  ggplot(aes(x = RT, y = Performance, color = arms,
             shape = context, group = arms)) +
  geom_line(size = 1) +
  geom_point(size = 4) +
  labs(title = "RT x Accuracy",
       x = "RT (ms)",
       y = "Accuracy") +
  theme_cowplot()

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  acc_cond_inter + theme(legend.box.margin = margin(0, 0, 0, 12))
)
acc_RT_x_condition <- cowplot::plot_grid(acc_cond_inter + theme(legend.position = "none"),
                                lgRT_cond_inter + theme(legend.position = "none"),
                                labels = c('A', 'B'),
                                ncol = 2)

acc_RT_condition <- cowplot::plot_grid(acc_RT_x_condition, legend, rel_widths = c(2, .2))

## trait x accuracy
accuracy_trait <- accuracy_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(Performance = mean(Performance)) %>% # individual level
  left_join(traits_data, by = "Participant Private ID")

acc_IU <- accuracy_trait %>% 
  ggplot(aes(x = IU, y = Performance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

acc_IM <- accuracy_trait %>% 
  ggplot(aes(x = IM, y = Performance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

acc_anx <- accuracy_trait %>% 
  ggplot(aes(x = Anxi, y = Performance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

acc_risk <- accuracy_trait %>% 
  ggplot(aes(x = RA, y = Performance)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

acc_trait_plot <- cowplot::plot_grid(acc_IU, acc_IM, acc_anx, acc_risk,
                            labels = c('A', 'B', 'C', 'D'),
                            ncol = 4)

acc_trait_glm <- accuracy_trait %>% 
  select(-`Participant Private ID`) %>% 
  mutate(across(-Performance, scale)) %>% # for standardised coefficients
  as.data.frame() %>%
  glm(Performance ~ IU + IM + Anxi + RA, data = ., family = "binomial")
print_table(acc_trait_glm,
            file = "step2_descriptive_statistics/output/acc_trait_glm.doc")

# by context
traits_acc_context <- accuracy_data %>%
  left_join(traits_data, by = "Participant Private ID") %>%
  gather(key = "trait", value = "value", IU:RA) %>%
  ggplot(aes(x = value, y = Performance, color = trait, shape = context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_grid(context~trait, scales = "free_x") +
  labs(
    x = "Trait",
    y = "Correct Arm Selection Rate"
  )

# trials level
correct_trait_glmm <- correct_data %>% 
  left_join(traits_data, by = "Participant Private ID") %>% 
  mutate(across(IU:RA, scale), # for standardised coefficients
         context = case_when(
           grepl("Win", context) ~ -0.5,
           grepl("Lose", context) ~ 0.5,
         ),
         arms = case_when(
           grepl("r", arms) ~ -0.5,
           grepl("S", arms) ~ 0.5
         )) %>% 
  glmer(correct ~ (IU + IM + Anxi + RA) * (context + arms) + (1|`Participant Private ID`),
      data = ., family = "binomial")
summary(correct_trait_glmm)
correct_trait_fig <- plot_model(correct_trait_glmm,
                                title = "Choosing the Correct Arm",
                                show.values = TRUE,
                                value.offset = 0.4,
                                sort.est = TRUE) +
  theme_bw() +
  ylim(0.8, 1.35)

correct_trait_Fig <- cowplot::plot_grid(correct_trait_fig,
                                        traits_acc_context,
                                        rel_widths = c(1, 3),
                                        labels = c('A', 'B'))

## trait x RT
RT_trait <- RT_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(`Mean RT` = mean(`Reaction Time`),
            `Mean lgRT` = mean(lgRT)) %>%
  left_join(traits_data, by = "Participant Private ID")

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

RT_risk <- RT_trait %>%
  ggplot(aes(x = RA, y = `Mean RT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

RT_trait_plot <- cowplot::plot_grid(RT_IU, RT_IM, RT_anx, RT_risk,
                           labels = c('E', 'F', 'G', 'H'),
                           ncol = 4)

lgRT_IU <- RT_trait %>%
  filter(`Mean lgRT` > 5) %>% # remove outliers
  ggplot(aes(x = IU, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_IM <- RT_trait %>%
  filter(`Mean lgRT` > 5) %>% # remove outliers
  ggplot(aes(x = IM, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_anx <- RT_trait %>%
  filter(`Mean lgRT` > 5) %>% # remove outliers
  ggplot(aes(x = Anxi, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_risk <- RT_trait %>%
  filter(`Mean lgRT` > 5) %>% # remove outliers
  ggplot(aes(x = RA, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_trait_plot <- cowplot::plot_grid(lgRT_IU, lgRT_IM, lgRT_anx, lgRT_risk,
                             labels = c('E', 'F', 'G', 'H'),
                             ncol = 4)

RT_trait_glm <- RT_trait %>% 
  glm(`Mean RT` ~ IU + IM + Anxi + RA,
      data = ., family = Gamma(link = "log")) # Gamma distribution
GLM_summary(RT_trait_glm)
# original RT is marginal significant in IU (p = 0.095) and IM (p = 0.098) (without RA)
lgRT_trait_lm <- RT_trait %>% 
  filter(`Mean lgRT` > 5) %>% # remove outliers
  lm(`Mean lgRT` ~ IU + IM + Anxi + RA, data = .)
print_table(lgRT_trait_lm,
            file = "step2_descriptive_statistics/output/lgRT_trait_lm.doc")
# log RT is not significant in IM and Anxi

acc_trait_RT_plot <- cowplot::plot_grid(acc_trait_plot, RT_trait_plot, ncol = 1)
acc_trait_lgRT_plot <- cowplot::plot_grid(acc_trait_plot, lgRT_trait_plot, ncol = 1)

# with outlier
lgRT_trait_IU_outlier <- RT_trait %>% 
  ggplot(aes(x = IU, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_trait_IM_outlier <- RT_trait %>%
  ggplot(aes(x = IM, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_trait_anx_outlier <- RT_trait %>%
  ggplot(aes(x = Anxi, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_trait_risk_outlier <- RT_trait %>%
  ggplot(aes(x = RA, y = `Mean lgRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lgRT_trait_plot_outlier <- cowplot::plot_grid(lgRT_trait_IU_outlier, lgRT_trait_IM_outlier, lgRT_trait_anx_outlier, lgRT_trait_risk_outlier,
                                     labels = c('A', 'B', 'C', 'D'),
                                     ncol = 4)
# by context
traits_lgRT_context <- RT_data %>%
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(`Mean lgRT` = mean(lgRT)) %>%
  left_join(traits_data, by = "Participant Private ID") %>%
  gather(key = "trait", value = "value", IU:RA) %>%
  filter(`Mean lgRT` > 4.5) %>% # remove outliers
  ggplot(aes(x = value, y = `Mean lgRT`, color = trait, shape = context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_grid(context~trait, scales = "free_x") +
  labs(
    x = "Trait",
    y = "Mean Log RT"
  )

# trials level
lgRT_trait_glmm <- RT_data %>% 
  left_join(traits_data, by = "Participant Private ID") %>% 
  mutate(across(lgRT:RA, scale), # for standardised coefficients
         context = case_when(    # do not standardise indicator variables
           grepl("Win", context) ~ -0.5,
           grepl("Lose", context) ~ 0.5,
         ),
         arms = case_when(
           grepl("r", arms) ~ -0.5,
           grepl("S", arms) ~ 0.5
         )
        ) %>% 
  glmer(`Reaction Time` ~ (IU + IM + Anxi + RA) * (context + arms) +
          (1|`Participant Private ID`),
        data = ., family = Gamma(link = "log")) # Gamma distribution
summary(lgRT_trait_glmm)

lgRT_trait_fig <- plot_model(lgRT_trait_glmm,
                             type = "est",
                             title = "Log RT by Trait",
                             show.values = TRUE,
                             value.offset = 0.4,
                             sort.est = TRUE,
                             axis.title = "Ratio") +
  theme_bw() +
  ylim(0.89, 1.11)

lgRT_trait_Fig <- cowplot::plot_grid(lgRT_trait_fig,
                                    traits_lgRT_context,
                                    rel_widths = c(1, 3),
                                    labels = c('A', 'B'))

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
ggsave("step2_descriptive_statistics/output/trait_accuracy_RT_plot.png", acc_trait_RT_plot, width = 12, height = 6)
ggsave("step2_descriptive_statistics/output/trait_accuracy_lgRT_plot.png", acc_trait_lgRT_plot, width = 12, height = 6)
ggsave("step2_descriptive_statistics/output/lgRT_trait_plot_outlier.png", lgRT_trait_plot_outlier, width = 12, height = 3)
ggsave("step2_descriptive_statistics/output/lgRT_trait_plot_nooutlier.png", lgRT_trait_plot, width = 12, height = 3)
ggsave("step2_descriptive_statistics/output/acc_RT_condition.png", acc_RT_condition, width = 13, height = 6)
ggsave("step2_descriptive_statistics/output/traits_acc_context.png", traits_acc_context, width = 18, height = 6)
ggsave("step2_descriptive_statistics/output/correct_trait_Fig.png", correct_trait_Fig, width = 18, height = 6)
ggsave("step2_descriptive_statistics/output/correct_trait_fig.png", correct_trait_fig, width = 6, height = 6)
ggsave("step2_descriptive_statistics/output/lgRT_trait_Fig.png", lgRT_trait_Fig, width = 18, height = 6)
ggsave("step2_descriptive_statistics/output/lgRT_trait_ffig.png", lgRT_trait_fig, width = 6, height = 6) # why _fig will replace _Fig????
ggsave("step2_descriptive_statistics/output/traits_lgRT_context.png", traits_lgRT_context, width = 18, height = 6)

# Save the models
save(acc_trait_glm, lgRT_trait_lm, file = "step2_descriptive_statistics/output/acc_trait_RT_lm.RData")