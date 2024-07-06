# Remove uneligible blocks
# input: main task data
# output: pass inclusive criteria (inclusion.csv), questionnaire scores (questionnaire_score.csv)

# List of required packages
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

# read main data
main_data <- read_csv("raw_data/data_exp_172913-v29/data_exp_172913-v29_task-o8v9.csv") %>% 
  head(-1) %>% # remove the last row
  filter(`Response Type` != "info") %>%  # remove the information rows
  filter(Display == "Trial") %>% # extract only trial data
  filter(`Response Type` == "response") %>% # extract only response data
  mutate(`Event Index` = as.numeric(`Event Index`)) 

# identify participants with more than 240 trials
issue_participants <- main_data %>% 
  group_by(`Participant Private ID`) %>%
  summarise(n = n()) %>%
  filter(n > 240) %>%
  select(`Participant Private ID`)

## remove duplicated rows, like 11376357
# remove multiple response
second_response <- main_data %>% 
  group_by(`Participant Private ID`, `Trial Number`) %>%
  filter(n()>1) %>% 
  filter(`Event Index` != min(`Event Index`)) # keep first response

main_data <- anti_join(main_data, second_response) # remove

## remove omission and too fast trials
# have a look on distribution of response time
too_fast <- main_data %>% 
  filter(`Reaction Time` < 150) %>% # remove trials with response time less than 100ms
  select(`Participant Private ID`, `Reaction Time`)

hist_too_fast <- ggplot(too_fast, aes(x = `Reaction Time`)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Too fast trials distribution", x = "RT (ms)", y = "Frequency") +
  theme_minimal()
ggsave("step1_data_wrangling/output/hist_too_fast.png", hist_too_fast, width = 6, height = 4)

hist_RT <- ggplot(main_data, aes(x = `Reaction Time`)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "RT distribution", x = "RT (ms)", y = "Frequency") +
  geom_vline(xintercept = 150, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red") +
  xlim(0, 4000) +
  theme_minimal()
ggsave("step1_data_wrangling/output/hist_RT.png", hist_RT, width = 6, height = 4)

# read excluded participants
excluded_participants <- read_csv("step1_data_wrangling/output/inclusion.csv") %>% 
  filter(pass == FALSE)

# consider 100ms as the threshold for too fast trials and treat them as omission
cleaned_data <- main_data %>%
  mutate(Response = ifelse(`Reaction Time` < 100, "No Response", Response)) %>% 
  group_by(`Participant Private ID`, `Spreadsheet: block_flag`) %>%
  mutate(block_omit = sum(Response == "No Response")) %>% # count omission trials and too fast trials
  ungroup(`Participant Private ID`, `Spreadsheet: block_flag`) %>% 
  filter(block_omit < 5) %>% # remove block with less than 8 valid response trials
  filter(Response != "No Response") %>%  # remove omission trials
  anti_join(excluded_participants, by = join_by(`Participant Private ID`)) # remove excluded participants

# save cleaned data
write_csv(cleaned_data, "step1_data_wrangling/output/cleaned_data.csv")