# Data wrangling for Kalman filter
# input: cleaned_data.csv
# output: kalman_data.csv

required_packages <- c("tidyverse")

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

cleaned_data <- read_csv("step1_data_wrangling/output/cleaned_data.csv")
questionnaire <- read_csv("step1_data_wrangling/output/questionnaire_score.csv") %>% 
  # mutate(across(-1, scale))
  transmute(
    `Participant Private ID` = `Participant Private ID`,
    IIU = scale(IIU),
    PIU = scale(PIU),
    IU  = scale(IU),
    IM  = scale(`Overall impulsiveness`),
    Anx = scale(Overall_anxiety),
  )

kalman_data <- cleaned_data %>%
  transmute(
    `Participant Private ID` = `Participant Private ID`,
    trial_number = `Trial Number`,
    block_number = `Spreadsheet: block_flag`,
    payoff = `Store: trial_payoff`,
    response = `Object Name`,
    left = `Spreadsheet: Left SR`,
    right = `Spreadsheet: Right SR`,
    payoff_left = `Spreadsheet: Left Payoff`,
    payoff_right = `Spreadsheet: Right Payoff`,
    frame = case_when(
      grepl("Win", `Spreadsheet: Condition`) ~ "Win",
      grepl("Lose", `Spreadsheet: Condition`) ~ "Lose",
    ),
    arms = case_when(
      grepl("S", `Spreadsheet: Condition`) ~ "SR",
      grepl("r", `Spreadsheet: Condition`) ~ "rR",
    )
         ) %>% 
  mutate(
    response = case_when(
      grepl("Left", response) ~ "Left",
      grepl("Right", response) ~ "Right"
      ),
    choice = case_when(
      response == "Left" ~ 1,
      response == "Right" ~ 2
    ),
    trial = trial_number %% 12 + 1,
  ) %>% 
  group_by(`Participant Private ID`, block_number) %>%
  mutate(
    value_left = mean(payoff_left),
    value_right = mean(payoff_right),
  ) %>%
  ungroup() %>% 
  left_join(questionnaire, by = "Participant Private ID") %>%
  transmute(ID = as.integer(factor(`Participant Private ID`)))  # transform to 1, 2, 3

write_csv(kalman_data, "step1_data_wrangling/output/kalman_data.csv")