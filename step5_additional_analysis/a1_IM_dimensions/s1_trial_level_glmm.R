# Additional analysis: IM dimensions
# Exploration of whether the better performance is correlated with specific IM dimensions

# List of required packages
required_packages <- c("tidyverse","lme4","bruceR","cowplot")

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

data_dir <- "raw_data/data_exp_172913-v29/"
output_dir <- "step5_additional_analysis/a1_IM_dimensions/output/"

# SUPPS_P
SUPPS_node_key <- c("questionnaire-ubbx","questionnaire-5hzi","questionnaire-x6nt","questionnaire-kjpo","questionnaire-x733","questionnaire-8zpx")
SUPPS_files <- paste0(data_dir,"data_exp_172913-v29_", SUPPS_node_key,".csv")
SUPPS_data_list <- map(SUPPS_files, read_csv)

# Ensure all columns have the same data type, e.g., converting all to character
SUPPS_data_list <- map(SUPPS_data_list, ~mutate_all(.x, as.character)) %>% 
  map(~head(.x, -1)) # remove the last row

combined_SUPPS <- bind_rows(SUPPS_data_list) %>% 
  filter(`Response Type` != "info") %>% # remove the information row
  mutate(`Event Index` = as.numeric(`Event Index`)) %>% # for compatible types
  filter(`Event Index` < 46)
# Participant 11377307 has network issue, so we need to remove duplication

################### Sensation Seeking ###################
SUPPS_sensation <- combined_SUPPS %>% 
  filter(Key == "10) I quite enjoy taking risks.-quantised" |
         Key == "15) I welcome new and exciting experiences and sensations, even if they are a little frightening and unconventional.-quantised" |
         Key == "17) I would like to learn to fly an airplane.-quantised" |
         Key == "19) I would enjoy the sensation of skiing very fast down a high mountain slope.-quantised"
         ) %>% 
  select(`Participant Private ID`, Response, Key) %>% 
  group_by(`Participant Private ID`) %>% 
  summarise(Response = sum(as.numeric(Response)), Key = "SUPPS_sensation") %>% 
  spread(Key, Response) %>% 
  mutate(`Participant Private ID` = as.double(`Participant Private ID`),
         SUPPS_sensation = scale(SUPPS_sensation)[,1]) # standardise

cleaned_data <- read_csv("step1_data_wrangling/output/cleaned_data.csv")

# Trial level analysis
correct_data <- cleaned_data %>% # trial level
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
  filter(`Spreadsheet: Left Correct` == 1 | `Spreadsheet: Right Correct` == 1) %>% # remove the trials with both arms incorrect
  transmute(`Participant Private ID`, context, arms,
            correct = `Store: trial_correct`) %>% 
  mutate(context = factor(context, levels = c("Lose", "Win")),
         arms = factor(arms, levels = c("SR", "rR")))

data <- correct_data %>% 
  left_join(SUPPS_sensation, by = "Participant Private ID")

correct_sensation_glmm <- glmer(correct ~ SUPPS_sensation * (context + arms) + (1|`Participant Private ID`),
                             data = data, family = "binomial")
HLM_summary(correct_sensation_glmm)

# Fig
im_values <- seq(
  from = min(data$SUPPS_sensation),
  to = max(data$SUPPS_sensation),
  length.out = 10
)

SS_post_hoc <- emtrends(
  correct_sensation_glmm,
  ~ SUPPS_sensation | context,
  var = "SUPPS_sensation",
  at = list(context = c("Win", "Lose"), SUPPS_sensation = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4) %>% # 1 Win 2 Lose
  format(scientific = F)

correct_sensation_inter <- emmeans(
  correct_sensation_glmm,
  specs = ~ SUPPS_sensation | context,
  type = "response",
  at = list(
    SUPPS_sensation = im_values,
    context = c("Lose", "Win")
  )
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = SUPPS_sensation, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  # ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection") +
  xlab("← Less Sensation Seeking               More Sensation Seeking →") +
  theme(legend.position = "top") +
  scale_fill_discrete(
    labels = c(
      "Win" = paste0("p = ", SS_post_hoc[1]),
      "Lose"  = paste0("p = ", SS_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL, order = 1),
         fill   = guide_legend(title = NULL))

################### Negative Urgency ###################
SUPPS_NU <- combined_SUPPS %>% 
  filter(Key == "7) When I feel bad, I will often do things I later regret in order to make myself feel better now.-quantised" |
         Key == "9) Sometimes when I feel bad, I can't seem to stop what I am doing even though it is making me feel worse.-quantised" |
         Key == "14) When I am upset I often act without thinking.-quantised" |
         Key == "16) When I feel rejected, I will often say things that I later regret.-quantised"
  ) %>% 
  select(`Participant Private ID`, Response, Key) %>% 
  group_by(`Participant Private ID`) %>% 
  summarise(Response = sum(as.numeric(Response)), Key = "SUPPS_NU") %>% 
  spread(Key, Response) %>% 
  mutate(`Participant Private ID` = as.double(`Participant Private ID`),
         SUPPS_NU = scale(SUPPS_NU)[,1]) # standardise

# Trial level analysis
data_NU <- correct_data %>% 
  left_join(SUPPS_NU, by = "Participant Private ID")
correct_NU_glmm <- glmer(correct ~ SUPPS_NU * (context + arms) + (1|`Participant Private ID`),
                             data = data_NU, family = "binomial")
HLM_summary(correct_NU_glmm)

# Fig
im_values_NU <- seq(
  from = min(data_NU$SUPPS_NU),
  to = max(data_NU$SUPPS_NU),
  length.out = 10
)

NU_post_hoc <- emtrends(
  correct_NU_glmm,
  ~ SUPPS_NU | context,
  var = "SUPPS_NU",
  at = list(context = c("Win", "Lose"), SUPPS_NU = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4) %>% # 1 Win 2 Lose
  format(scientific = F)

correct_NU_inter <- emmeans(
  correct_NU_glmm,
  specs = ~ SUPPS_NU | context,
  type = "response",
  at = list(
    SUPPS_NU = im_values_NU,
    context = c("Lose", "Win")
  )
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = SUPPS_NU, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  # ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection") +
  xlab("← Less Negative Urgency            More Negative Urgency →") +
  theme(legend.position = "top") +
  scale_fill_discrete(
    labels = c(
      "Win" = paste0("p = ", NU_post_hoc[1]),
      "Lose"  = paste0("p = ", NU_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL, order = 1),
         fill   = guide_legend(title = NULL))

################### Positve Urgency ###################
SUPPS_PU <- combined_SUPPS %>%
  filter(Key == "3) When I am in great mood, I tend to get into situations that could cause me problems.-quantised" |
         Key == "11) I tend to lose control when I am in a great mood.-quantised" |
         Key == "18) Others are shocked or worried about the things I do when I am feeling very excited.-quantised" |
         Key == "21) I tend to act without thinking when I am really excited.-quantised"
  ) %>% 
  select(`Participant Private ID`, Response, Key) %>% 
  group_by(`Participant Private ID`) %>% 
  summarise(Response = sum(as.numeric(Response)), Key = "SUPPS_PU") %>% 
  spread(Key, Response) %>% 
  mutate(`Participant Private ID` = as.double(`Participant Private ID`),
         SUPPS_PU = scale(SUPPS_PU)[,1]) # standardise

# Trial level analysis
data_PU <- correct_data %>% 
  left_join(SUPPS_PU, by = "Participant Private ID")
correct_PU_glmm <- glmer(correct ~ SUPPS_PU * (context + arms) + (1|`Participant Private ID`),
                         data = data_PU, family = "binomial")
HLM_summary(correct_PU_glmm)
# Fig
im_values_PU <- seq(
  from = min(data_PU$SUPPS_PU),
  to = max(data_PU$SUPPS_PU),
  length.out = 10
)

PU_post_hoc <- emtrends(
  correct_PU_glmm,
  ~ SUPPS_PU | context,
  var = "SUPPS_PU",
  at = list(context = c("Win", "Lose"), SUPPS_PU = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4) %>% # 1 Win 2 Lose
  format(scientific = F)

correct_PU_inter <- emmeans(
  correct_PU_glmm,
  specs = ~ SUPPS_PU | context,
  type = "response",
  at = list(
    SUPPS_PU = im_values_PU,
    context = c("Lose", "Win")
  )
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = SUPPS_PU, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  # ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection") +
  xlab("← Less Positive Urgency            More Positive Urgency →") +
  theme(legend.position = "top") +
  scale_fill_discrete(
    labels = c(
      "Win" = paste0("p = ", PU_post_hoc[1]),
      "Lose"  = paste0("p = ", PU_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL, order = 1),
         fill   = guide_legend(title = NULL))

################### Lack of Perseverance ###################
SUPPS_LPer <- combined_SUPPS %>%
  filter(Key == "1) I generally like to see things through to the end.-quantised" |
         Key == "4) Unfinished tasks really bother me.-quantised" |
         Key == "8) Once I get going on something I hate to stop.-quantised" |
         Key == "12) I finish what I start.-quantised"
  ) %>% 
  select(`Participant Private ID`, Response, Key) %>% 
  mutate(Response = as.numeric(Response), # reverse scoring
         Response = ifelse(Key == "1) I generally like to see things through to the end.-quantised", 5 - Response, Response),
         Response = ifelse(Key == "4) Unfinished tasks really bother me.-quantised", 5 - Response, Response),
         Response = ifelse(Key == "8) Once I get going on something I hate to stop.-quantised", 5 - Response, Response)
  ) %>% 
  group_by(`Participant Private ID`) %>% 
  summarise(Response = sum(as.numeric(Response)), Key = "SUPPS_LPer") %>% 
  spread(Key, Response) %>% 
  mutate(`Participant Private ID` = as.double(`Participant Private ID`),
         SUPPS_LPer = scale(SUPPS_LPer)[,1]) # standardise
# Trial level analysis
data_LPer <- correct_data %>% 
  left_join(SUPPS_LPer, by = "Participant Private ID")
correct_LPer_glmm <- glmer(correct ~ SUPPS_LPer * (context + arms) + (1|`Participant Private ID`),
                         data = data_LPer, family = "binomial")
HLM_summary(correct_LPer_glmm)
# Fig
im_values_LPer <- seq(
  from = min(data_LPer$SUPPS_LPer),
  to = max(data_LPer$SUPPS_LPer),
  length.out = 10
)

LPer_post_hoc <- emtrends(
  correct_LPer_glmm,
  ~ SUPPS_LPer | context,
  var = "SUPPS_LPer",
  at = list(context = c("Win", "Lose"), SUPPS_LPer = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4) %>% # 1 Win 2 Lose
  format(scientific = F)

correct_LPer_inter <- emmeans(
  correct_LPer_glmm,
  specs = ~ SUPPS_LPer | context,
  type = "response",
  at = list(
    SUPPS_LPer = im_values_LPer,
    context = c("Lose", "Win")
  )
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = SUPPS_LPer, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  # ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection") +
  xlab("← Less Lack of Perseverance            More Lack of Perseverance →") +
  theme(legend.position = "top") +
  scale_fill_discrete(
    labels = c(
      "Win" = paste0("p = ", LPer_post_hoc[1]),
      "Lose"  = paste0("p = ", LPer_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL, order = 1),
         fill = guide_legend(title = NULL))

################### Lack of Premeditation ###################
SUPPS_LPre <- combined_SUPPS %>%
  filter(Key == "2) My thinking is usually careful and purposeful.-quantised" |
         Key == "6) I like to stop and think things over before I do them.-quantised" |
         Key == "13) I tend to value and follow a rational, “sensible” approach to things.-quantised" |
         Key == "20) I usually think carefully before doing anything.-quantised"
  ) %>% 
  select(`Participant Private ID`, Response, Key) %>% 
  mutate(Response = as.numeric(Response), # reverse scoring
         Response = ifelse(Key == "6) I like to stop and think things over before I do them.-quantised", 5 - Response, Response),
         Response = ifelse(Key == "13) I tend to value and follow a rational, “sensible” approach to things.-quantised", 5 - Response, Response),
         Response = ifelse(Key == "20) I usually think carefully before doing anything.-quantised", 5 - Response, Response)
  ) %>%
  group_by(`Participant Private ID`) %>% 
  summarise(Response = sum(as.numeric(Response)), Key = "SUPPS_LPre") %>% 
  spread(Key, Response) %>% 
  mutate(`Participant Private ID` = as.double(`Participant Private ID`),
         SUPPS_LPre = scale(SUPPS_LPre)[,1]) # standardise
# Trial level analysis
data_LPre <- correct_data %>%
  left_join(SUPPS_LPre, by = "Participant Private ID")
correct_LPre_glmm <- glmer(correct ~ SUPPS_LPre * (context + arms) + (1|`Participant Private ID`),
                          data = data_LPre, family = "binomial")
HLM_summary(correct_LPre_glmm)
# Fig
im_values_LPre <- seq(
  from = min(data_LPre$SUPPS_LPre),
  to = max(data_LPre$SUPPS_LPre),
  length.out = 10
)

LPre_post_hoc <- emtrends(
  correct_LPre_glmm,
  ~ SUPPS_LPre | context,
  var = "SUPPS_LPre",
  at = list(context = c("Win", "Lose"), SUPPS_LPre = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4) %>% # 1 Win 2 Lose
  format(scientific = F)

correct_LPre_inter <- emmeans(
  correct_LPre_glmm,
  specs = ~ SUPPS_LPre | context,
  type = "response",
  at = list(
    SUPPS_LPre = im_values_LPre,
    context = c("Lose", "Win")
  )
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = SUPPS_LPre, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  # ylim(0.4, 0.8) +
  xlim(-3, 3) +
  ylab("Probability of Correct Arm Selection") +
  xlab("← Less Lack of Premeditation            More Lack of Premeditation →") +
  theme(legend.position = "top") +
  scale_fill_discrete(
    labels = c(
      "Win" = paste0("p = ", LPre_post_hoc[1]),
      "Lose"  = paste0("p = ", LPre_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL, order = 1),
         fill   = guide_legend(title = NULL))

# Save all plots
ifelse(!dir.exists(file.path(output_dir)), dir.create(file.path(output_dir)), FALSE)
ggsave(paste0(output_dir, "correct_sensation_inter.png"), correct_sensation_inter, width = 7, height = 6)
ggsave(paste0(output_dir, "correct_NU_inter.png"), correct_NU_inter, width = 7, height = 6)
ggsave(paste0(output_dir, "correct_PU_inter.png"), correct_PU_inter, width = 7, height = 6)
ggsave(paste0(output_dir, "correct_LPer_inter.png"), correct_LPer_inter, width = 7, height = 6)
ggsave(paste0(output_dir, "correct_LPre_inter.png"), correct_LPre_inter, width = 7, height = 6)
