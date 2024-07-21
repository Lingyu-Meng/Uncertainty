# Remove invalid trials and participants
# input: main task data, inclusion.csv
# output: cleaned_data.csv

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

## remove omission
# read excluded participants
excluded_participants <- read_csv("step1_data_wrangling/output/inclusion.csv") %>% 
  filter(pass == FALSE)

cleaned_data <- main_data %>%
  group_by(`Participant Private ID`, `Spreadsheet: block_flag`) %>%
  mutate(block_omit = sum(Response == "No Response"), # count omission trials
         block_sele = sum(Response == "greenBox.png")) %>% # count single arm selection
  ungroup(`Participant Private ID`, `Spreadsheet: block_flag`) %>% 
  filter(block_omit < 5) %>% # remove block with less than 8 valid response trials
  filter(Response != "No Response") %>%  # remove omission trials
  filter(block_sele != 0 & block_sele != 12 - block_omit) %>% # remove blocks with selected one arm only
  anti_join(excluded_participants, by = join_by(`Participant Private ID`)) # remove excluded participants

# save cleaned data
write_csv(cleaned_data, "step1_data_wrangling/output/cleaned_data.csv")