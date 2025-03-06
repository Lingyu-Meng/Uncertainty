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
                       "sjPlot", # plot_model
                       "ggeffects") # ggpredict

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
         lnRT = log(`Reaction Time`))

# RT
RT_hist <- RT_data %>% 
  ggplot(aes(x = `Reaction Time`)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "RT Distribution",
       x = "RT (ms)",
       y = "Frequency") +
  theme_cowplot()
lnRT_hist <- RT_data %>% 
  ggplot(aes(x = lnRT)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Log RT Distribution",
       x = "Log RT",
       y = "Frequency") +
  theme_cowplot()
RT_dist <- cowplot::plot_grid(RT_hist, lnRT_hist, ncol = 1)

## Model-free analysis without traits
# RT x Condition
# calculate the mean RT in same condition per participants
mean_RT <- RT_data %>% 
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(`Mean RT` = mean(`Reaction Time`),
            `Mean lnRT` = mean(lnRT)) %>%
  ungroup(`Participant Private ID`, context, arms)

rain_meanlnRT_context_arms <- mean_RT %>% 
  mutate(`Participant Private ID` = ifelse(
    arms == "SR", `Participant Private ID` * 10,
    `Participant Private ID`) # to separate the two conditions
  ) %>%
  ggplot(aes(x = context, y = `Mean RT`, fill = arms, color = arms)) +
  geom_rain(rain.side = 'f2x2', id.long.var = "Participant Private ID", alpha = 0.5) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              y_position = 3.1,
              vjust = 2.3, colour = "#00BFC4") +
  geom_signif(y_position = 3.16, xmin = 0.85, xmax = 0.95,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 3.16, xmin = 2.05, xmax = 2.15,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  xlab("Context") +
# legend title
  labs(color = "Arms",
       fill  = "Arms") +
  theme_cowplot() +
  scale_y_log10(breaks = c(200, 300, 500, 1000, 1500))

rain_meanRT_context_arms <- mean_RT %>% 
  mutate(`Participant Private ID` = ifelse(
    arms == "SR", `Participant Private ID` * 10,
    `Participant Private ID`) # to separate the two conditions
  ) %>%
  ggplot(aes(x = context, y = `Mean RT`, fill = arms, color = arms)) +
  geom_rain(rain.side = 'f2x2', id.long.var = "Participant Private ID", alpha = 0.5) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              y_position = 1350,
              vjust = 2.3, colour = "#00BFC4") +
  geom_signif(y_position = 1390, xmin = 0.85, xmax = 0.95,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 1390, xmin = 2.05, xmax = 2.15,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  theme_cowplot()

# RT Generalised Linear Mixed Model (RT ~ context:arms + (1|Participant Private ID)
glmm_RT <- RT_data %>% 
  glmer(`Reaction Time` ~ context*arms + (1|`Participant Private ID`),
        data = ., family = Gamma(link = "log")) # Gamma distribution
summary(glmm_RT) # WIN SR as baseline
lnRT_posthoc <- lsmeans(glmm_RT, pairwise ~ context:arms, adjust = "tukey") # post hoc test
# Only Win SR - Win rR and Lose SR - Lose rR is not significant
# Or conduct ANOVA, which may has less power, but the results are the same
# RT_aov <- RT_data %>%
#   aov(`Reaction Time` ~ context*arms + Error(`Participant Private ID`), data = .)
# summary(RT_aov) # main effect of context only


# Visiualise the HLM results
vis_data_lnRT <- lnRT_posthoc$lsmeans %>% 
  as.data.frame() %>%
  mutate(lnRT = lsmean,
         lnRT_LCI = asymp.LCL,
         lnRT_UCI = asymp.UCL)

# Plot the interaction
lnRT_cond_inter <- vis_data_lnRT %>% 
  ggplot(aes(x = context, y = lnRT, color = arms, group = arms)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = lnRT_LCI, ymax = lnRT_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              y_position = 6.655,
              annotation = c("***"), tip_length = 0) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("***"), tip_length = 0,
              y_position = 6.653,
              vjust = 2.3, colour = "#00BFC4") +
  geom_signif(y_position = 6.65, xmin = 0.95, xmax = 1.05,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  geom_signif(y_position = 6.65, xmin = 1.95, xmax = 2.05,
              annotation = c("NS."), tip_length = 0,
              colour = "black") +
  labs(color = "Arms",
       y = "Log RT",
       x = "Context") +
  theme_cowplot()

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

## Performance 
# Load the Performance  data
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
            correct = `Store: trial_correct`) %>% 
  mutate(context = factor(context, levels = c("Win", "Lose")),
         arms = factor(arms, levels = c("SR", "rR")))

accuracy_data <- correct_data%>% # condition level
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(Performance = mean(correct)) %>%
  left_join(demo, by = "Participant Private ID")

acc_hist <- accuracy_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(performance = mean(Performance)) %>%
  ggplot(aes(x = performance)) +
  geom_histogram(binwidth = 0.02, fill = "skyblue", color = "black") +
  labs(title = "Performance  Distribution",
       x = "Performance ",
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
         performance_UCI = relogit(asymp.UCL)) 

# Plot the interaction
acc_cond_inter <- vis_data_acc %>% 
  ggplot(aes(x = context, y = performance, color = arms, group = arms)) +
  geom_line(size = 1) +
  geom_point(size = 3, position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin = performance_LCI, ymax = performance_UCI), width = 0.05,
                position=position_dodge(0.05)) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("NS."), tip_length = 0,
              y_position = 0.61) +
  geom_signif(comparisons = list(c("Win", "Lose")),
              annotation = c("*"), tip_length = 0,
              y_position = 0.608, colour = "#00BFC4",
              vjust = 2.3) +
  geom_signif(y_position = 0.607, xmin = 0.95, xmax = 1.05, 
              annotation = c("***"), tip_length = 0, 
              colour = "black") +
  geom_signif(y_position = 0.607, xmin = 1.95, xmax = 2.05, 
              annotation = c("***"), tip_length = 0, 
              colour = "black") +
  labs(color = "Arms",
       y = "Performance ",
       x = "Context"
       ) +
  theme_cowplot()

## RT x Performance  by context x arms
vis_data <- vis_data_acc %>% 
  transmute(context = context, arms = arms,
            Performance = performance,
            performance_LCI = performance_LCI,
            performance_UCI = performance_UCI) %>%
  left_join(vis_data_lnRT, by = c("context", "arms")) %>% 
  mutate(RT     = exp(lnRT),
         RT_LCI = exp(lnRT_LCI),
         RT_UCI = exp(lnRT_UCI))

RT_acc <- vis_data %>% 
  ggplot(aes(x = RT, y = Performance, color = context,
             shape = arms, group = context)) +
  geom_line(size = 1) +
  geom_point(size = 4) +
  labs(title = "RT x Performance ",
       x = "RT (ms)",
       y = "Performance ") +
  theme_cowplot()

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  acc_cond_inter + theme(legend.box.margin = margin(0, 0, 0, 12))
)
acc_RT_x_condition <- cowplot::plot_grid(acc_cond_inter + theme(legend.position = "none"),
                                lnRT_cond_inter + theme(legend.position = "none"),
                                labels = c('A', 'B'),
                                ncol = 2)

acc_RT_condition <- cowplot::plot_grid(acc_RT_x_condition, legend, rel_widths = c(2, .2))

## Model-free analysis with traits
## trait x Performance 
# individual level plot
accuracy_trait <- accuracy_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(Performance = mean(Performance)) %>%
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

# by context and arms
traits_acc_context <- correct_data %>% 
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(Performance = mean(correct)) %>%
  left_join(traits_data, by = "Participant Private ID") %>%
  gather(key = "trait", value = "value", IU:RA) %>%
  ggplot(aes(x = value, y = Performance, color = trait, shape = arms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_grid(context~trait, scales = "free_x") +
  labs(
    x = "Trait",
    y = "Correct Arm Selection Rate"
  )

# trials level analysis
correct_trait_data <- correct_data %>% 
  left_join(traits_data, by = "Participant Private ID") %>% 
  mutate(across(IU:RA, scale), # for standardised coefficients
         context = case_when(
           grepl("Win", context) ~ -0.5,
           grepl("Lose", context) ~ 0.5,
         ),
         arms = case_when(
           grepl("r", arms) ~ -0.5,
           grepl("S", arms) ~ 0.5
         ))

correct_trait_glmm <- glmer(correct ~ (IU + IM + Anxi + RA) * (context + arms) + (1|`Participant Private ID`),
                            data = correct_trait_data, family = "binomial")
HLM_summary(correct_trait_glmm)

correct_trait_glmm2 <- glmer(correct ~ (IUi + IUp + IM + Anxi + RA) * (context + arms) + (1|`Participant Private ID`),
                             data = correct_trait_data, family = "binomial")
HLM_summary(correct_trait_glmm2)

correct_trait_fig <- plot_model(correct_trait_glmm,
                                title = "Choosing the Correct Arm",
                                show.values = TRUE,
                                value.offset = 0.4,
                                sort.est = TRUE) +
  theme_bw() +
  ylim(0.8, 1.35)

test_data = tibble(IU = 0, IM = 0, Anxi = 0, RA = 0, context = 0, arms = 0,`Participant Private ID` = NA)

ggpredict(correct_trait_glmm, terms = "IU [0, 1]")
ggpredict(correct_trait_glmm, terms = c("IU [0, 1]", "context"))
ggpredict(correct_trait_glmm, terms = c("IM [0, 1]", "context"))

correct_trait_glmm_IU_effect <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("IU"),
           colors = "black") + # as colour is repersenting the context
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")
correct_trait_glmm_IU_inter <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("IU","context"))+ 
  scale_color_discrete(name = "Context", labels = c("Win", "Loss")) +
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")

correct_trait_glmm_IM_effect <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("IM"),
           colors = "black") +
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")
correct_trait_glmm_IM_inter <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("IM","context")) + 
  scale_color_discrete(name = "Context", labels = c("Win", "Loss")) +
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")

correct_trait_glmm_Anx_effect <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("Anxi"),
           colors = "black") +
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")
correct_trait_glmm_Anx_inter <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("Anxi","context")) + 
  scale_color_discrete(name = "Context", labels = c("Win", "Loss")) +
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")

correct_trait_glmm_RA_effect <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("RA"),
           colors = "black") +
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")
correct_trait_glmm_RA_inter <- plot_model(correct_trait_glmm,
           title = "Choosing the Correct Arm",
           type = "pred",
           terms = c("RA","context")) + 
  scale_color_discrete(name = "Context", labels = c("Win", "Loss")) +
  ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection")

correct_trait_Fig <- cowplot::plot_grid(correct_trait_fig,
                                        traits_acc_context,
                                        rel_widths = c(1, 3),
                                        labels = c('A', 'B'))

# extract a legend that is laid out horizontally
legend <- get_legend(
  correct_trait_glmm_IM_inter
)

results_Correct <- cowplot::plot_grid(correct_trait_fig,
                                      correct_trait_glmm_IU_effect,
                                      correct_trait_glmm_IU_inter + theme(legend.position="none"),
                                      correct_trait_glmm_IM_inter + theme(legend.position="none"),
                                      ncol = 2,
                                      labels = c('A', 'B', 'C', 'D')
                                      )
Results_Correct <- cowplot::plot_grid(results_Correct,
                                      legend, ncol = 2,
                                      rel_widths = c(1, .1)
                                      )

## traits x RT
# individual level plot
RT_trait <- RT_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(`Mean RT` = mean(`Reaction Time`),
            `Mean lnRT` = mean(lnRT)) %>%
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

lnRT_IU <- RT_trait %>%
  filter(`Mean lnRT` > 5) %>% # remove outliers
  ggplot(aes(x = IU, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_IM <- RT_trait %>%
  filter(`Mean lnRT` > 5) %>% # remove outliers
  ggplot(aes(x = IM, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_anx <- RT_trait %>%
  filter(`Mean lnRT` > 5) %>% # remove outliers
  ggplot(aes(x = Anxi, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_risk <- RT_trait %>%
  filter(`Mean lnRT` > 5) %>% # remove outliers
  ggplot(aes(x = RA, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_trait_plot <- cowplot::plot_grid(lnRT_IU, lnRT_IM, lnRT_anx, lnRT_risk,
                             labels = c('E', 'F', 'G', 'H'),
                             ncol = 4)

RT_trait_glm <- RT_trait %>% 
  glm(`Mean RT` ~ IU + IM + Anxi + RA,
      data = ., family = Gamma(link = "log")) # Gamma distribution
GLM_summary(RT_trait_glm)
# original RT is marginal significant in IU (p = 0.095) and IM (p = 0.098) (without RA)
lnRT_trait_lm <- RT_trait %>% 
  filter(`Mean lnRT` > 5) %>% # remove outliers
  lm(`Mean lnRT` ~ IU + IM + Anxi + RA, data = .)
print_table(lnRT_trait_lm,
            file = "step2_descriptive_statistics/output/lnRT_trait_lm.doc")
# log RT is not significant in IM and Anxi

acc_trait_RT_plot <- cowplot::plot_grid(acc_trait_plot, RT_trait_plot, ncol = 1)
acc_trait_lnRT_plot <- cowplot::plot_grid(acc_trait_plot, lnRT_trait_plot, ncol = 1)

# with outlier
lnRT_trait_IU_outlier <- RT_trait %>% 
  ggplot(aes(x = IU, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_trait_IM_outlier <- RT_trait %>%
  ggplot(aes(x = IM, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_trait_anx_outlier <- RT_trait %>%
  ggplot(aes(x = Anxi, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_trait_risk_outlier <- RT_trait %>%
  ggplot(aes(x = RA, y = `Mean lnRT`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()

lnRT_trait_plot_outlier <- cowplot::plot_grid(lnRT_trait_IU_outlier, lnRT_trait_IM_outlier, lnRT_trait_anx_outlier, lnRT_trait_risk_outlier,
                                     labels = c('A', 'B', 'C', 'D'),
                                     ncol = 4)
# by context and arms
traits_lnRT_context <- RT_data %>%
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(`Mean lnRT` = mean(lnRT)) %>%
  left_join(traits_data, by = "Participant Private ID") %>%
  gather(key = "trait", value = "value", IU:RA) %>%
  filter(`Mean lnRT` > 5) %>% # remove outliers
  ggplot(aes(x = value, y = `Mean lnRT`, color = trait, shape = arms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_grid(context~trait, scales = "free_x") +
  labs(
    x = "Trait",
    y = "Mean Log RT"
  )

# trials level analysis
lnRT_trait_glmm <- RT_data %>% 
  left_join(traits_data, by = "Participant Private ID") %>% 
  mutate(across(lnRT:RA, scale), # for standardised coefficients
         context = case_when(    # do not standardise indicator variables
           grepl("Win", context) ~ -0.5,
           grepl("Lose", context) ~ 0.5,
         ),
         arms = case_when(
           grepl("r", arms) ~ -0.5,
           grepl("S", arms) ~ 0.5
         )
        ) %>% 
  filter(`Reaction Time` > 150) %>% # remove outliers; ln(100) ≈ 5
  glmer(`Reaction Time` ~ (IU + IM + Anxi + RA) * (context + arms) +
          (1|`Participant Private ID`),
        data = ., family = Gamma(link = "log")) # Gamma distribution
HLM_summary(lnRT_trait_glmm)

ggpredict(lnRT_trait_glmm, terms = c("Anxi [0, 1]", "context"))

RT_traits_glmm_Ani_effect <- plot_model(lnRT_trait_glmm,
           title = "Interaction between anxiety and context on RT",
           type = "pred",
           terms = c("Anxi", "context"),
           show.values = TRUE,
           value.offset = 0.4,
           sort.est = TRUE) + 
  scale_color_discrete(name = "Context", labels = c("Win", "Loss"))

lnRT_trait_fig <- plot_model(lnRT_trait_glmm,
                             type = "est",
                             title = "Log RT by Trait",
                             show.values = TRUE,
                             value.offset = 0.4,
                             sort.est = TRUE,
                             axis.title = "Ratio") +
  theme_bw() +
  ylim(0.89, 1.11)

lnRT_trait_Fig <- cowplot::plot_grid(lnRT_trait_fig,
                                    traits_lnRT_context,
                                    rel_widths = c(1, 3),
                                    labels = c('A', 'B'))

Results_RT <- cowplot::plot_grid(lnRT_trait_fig,
                                 RT_traits_glmm_Ani_effect,
                                 labels = c('A', 'B'))

## traits and omission
data_with_omit <- read_csv("step1_data_wrangling/output/cleaned_data_with_omit.csv") %>% 
  transmute(`Participant Private ID`,
            omit = ifelse(Response == "No Response", 1, 0)) %>% 
  group_by(`Participant Private ID`) %>%
  summarise(omit_rate = mean(omit)) %>%
  left_join(traits_data, by = "Participant Private ID")

omit_traits_glm <- glm(omit_rate ~ IU + IM + Anxi + RA,
                       data = data_with_omit, family = "binomial")
GLM_summary(omit_traits_glm)

omit_traits_glm2 <- glm(omit_rate ~ IUi + IUp + IM + Anxi + RA,
                        data = data_with_omit, family = "binomial")
GLM_summary(omit_traits_glm2)

omission_by_traits <- data_with_omit %>% 
  gather(key = "trait", value = "value", IU:RA) %>%
  ggplot(aes(x = value, y = omit_rate, color = trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~trait, scales = "free_x") +
  labs(
    x = "Trait",
    y = "Omission Rate"
  )

# by context and arm
data_with_omit_condi <- read_csv("step1_data_wrangling/output/cleaned_data_with_omit.csv") %>% 
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
            omit = ifelse(Response == "No Response", 1, 0)) %>% 
  mutate(context = factor(context, levels = c("Win", "Lose")),
         arms = factor(arms, levels = c("SR", "rR")))

omission_traits_condi <- data_with_omit_condi %>% 
  group_by(`Participant Private ID`, context, arms) %>%
  summarise(omit_rate = mean(omit)) %>%
  left_join(traits_data, by = "Participant Private ID") %>% 
  gather(key = "trait", value = "value", IU:RA) %>%
  ggplot(aes(x = value, y = omit_rate, color = trait, shape = arms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_grid(context~trait, scales = "free_x") +
  labs(
    x = "Trait",
    y = "Omission Rate"
  )

# trials level analysis
# omit_trait_glmm <- data_with_omit_condi %>% 
#   left_join(traits_data, by = "Participant Private ID") %>%
#   glmer(omit ~ (IU + IM + Anxi + RA) * (context + arms) +
#           (1|`Participant Private ID`),
#         data = ., family = "binomial",
#         control = glmerControl(optCtrl = list(maxfun = 20000))) # as Model failed to converge with max|grad| = 0.024957
# summary(omit_trait_glmm)
# Give up as Model failed to converge again

# individual level analysis
# summarised_omit_traits_condi <- data_with_omit_condi %>% 
#   group_by(`Participant Private ID`, context, arms) %>%
#   summarise(omit_rate = mean(omit)) %>%
#   left_join(traits_data, by = "Participant Private ID")
#   
# omit_traits_glm_condi <- glm(omit_rate ~ (IU + IM + Anxi + RA) * (context + arms),
#                        data = summarised_omit_traits_condi, family = "binomial")
# GLM_summary(omit_traits_glm_condi)
# Give up as VIFs are too large

## Save the plots
ggsave("step2_descriptive_statistics/output/age_hist.png", age_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/gender_bar.png", gender_bar, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/age_gender_rain.png", age_gender_rain, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/age_2gender_rain.png", age_2gender_rain, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_hist.png", RT_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/lnRT_hist.png", lnRT_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_dist.png", RT_dist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/trait_corr.png", trait_cor$plot, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/rain_meanRT_context_arms.png", rain_meanRT_context_arms, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/rain_meanlnRT_context_arms.png", rain_meanlnRT_context_arms, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/response_rate_stack.png", response_rate_stack, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/accuracy_hist.png", acc_hist, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/accuracy_rain.png", accuracy_rain, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/accuracy_condition_interaction.png", acc_cond_inter, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_accuracy.png", RT_acc, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/trait_accuracy_RT_plot.png", acc_trait_RT_plot, width = 12, height = 6)
ggsave("step2_descriptive_statistics/output/trait_accuracy_lnRT_plot.png", acc_trait_lnRT_plot, width = 12, height = 6)
ggsave("step2_descriptive_statistics/output/lnRT_trait_plot_outlier.png", lnRT_trait_plot_outlier, width = 12, height = 3)
ggsave("step2_descriptive_statistics/output/lnRT_trait_plot_nooutlier.png", lnRT_trait_plot, width = 12, height = 3)
ggsave("step2_descriptive_statistics/output/acc_RT_condition.png", acc_RT_condition, width = 13, height = 6)
ggsave("step2_descriptive_statistics/output/traits_acc_context.png", traits_acc_context, width = 18, height = 6)
ggsave("step2_descriptive_statistics/output/correct_trait_Fig.png", correct_trait_Fig, width = 16, height = 4.8)
ggsave("step2_descriptive_statistics/output/correct_trait_ffig.png", correct_trait_fig, width = 6, height = 6)
ggsave("step2_descriptive_statistics/output/lnRT_trait_Fig.png", lnRT_trait_Fig, width = 16, height = 4.8)
ggsave("step2_descriptive_statistics/output/lnRT_trait_ffig.png", lnRT_trait_fig, width = 6, height = 6) # why _fig will replace _Fig????
ggsave("step2_descriptive_statistics/output/traits_lnRT_context.png", traits_lnRT_context, width = 18, height = 6)
ggsave("step2_descriptive_statistics/output/traits_omit_context.png", omission_traits_condi, width = 18, height = 6)
ggsave("step2_descriptive_statistics/output/correct_trait_glmm_IU_effect.png", correct_trait_glmm_IU_effect, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/correct_trait_glmm_IU_inter.png", correct_trait_glmm_IU_inter, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/correct_trait_glmm_IM_inter.png", correct_trait_glmm_IM_inter, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/RT_traits_glmm_Ani_effect.png", RT_traits_glmm_Ani_effect, width = 8, height = 6)
ggsave("step2_descriptive_statistics/output/Results_RT.png", Results_RT, width = 12.6, height = 6)
ggsave("step2_descriptive_statistics/output/Results_Correct.png", Results_Correct, width = 9.9, height = 9)

# Save the models
save(acc_trait_glm, lnRT_trait_lm, file = "step2_descriptive_statistics/output/acc_trait_RT_lm.RData")
saveRDS(rain_meanlnRT_context_arms,   "fig/fig2_panelD.rds")
saveRDS(correct_trait_glmm_IU_effect, "fig/fig4_panelA.rds")
saveRDS(correct_trait_glmm_IU_inter,  "fig/fig4_panelE.rds")
saveRDS(correct_trait_glmm_IM_effect, "fig/fig3_panelA.rds")
saveRDS(correct_trait_glmm_IM_inter,  "fig/fig3_panelE.rds")
saveRDS(correct_trait_glmm_Anx_effect,"fig/fig5_panelA.rds")
saveRDS(correct_trait_glmm_Anx_inter, "fig/fig5_panelE.rds")
saveRDS(correct_trait_glmm_RA_effect, "fig/fig6_panelA.rds")
saveRDS(correct_trait_glmm_RA_inter,  "fig/fig6_panelE.rds")