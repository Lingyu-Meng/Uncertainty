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

trial_effect <- cowplot::plot_grid(acc_plot, rt_plot, nrow = 1)

# Save the plot
ggsave("step2_descriptive_statistics/output/trial_effect.png", plot = trial_effect, width = 10, height = 5)