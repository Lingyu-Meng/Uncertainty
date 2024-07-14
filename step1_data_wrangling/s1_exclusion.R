# Combining questionnaires and attention checks
# input: raw data
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

data_dir <- "raw_data/data_exp_172913-v29/"
output_dir <- "step1_data_wrangling/output/"

#### Combining questionnaires
# IUS
IUS_node_key <- c("questionnaire-o9jb","questionnaire-alsx","questionnaire-zx7u","questionnaire-pwse","questionnaire-3bu9","questionnaire-hb2m")
IUS_files <- paste0(data_dir,"data_exp_172913-v29_", IUS_node_key,".csv")
IUS_data_list <- map(IUS_files, read_csv)

# Ensure all columns have the same data type, e.g., converting all to character
IUS_data_list <- map(IUS_data_list, ~mutate_all(.x, as.character)) %>% 
                 map(~head(.x, -1)) # remove the last row

combined_IUS <- bind_rows(IUS_data_list) %>% 
                filter(`Response Type` != "info") # remove the information row

IUS_score_short <- combined_IUS %>% 
  filter(Key == "score") %>% 
  select(`Participant Private ID`, Response, Question) %>% 
  spread(Question, Response)
  
attention_IUS <- combined_IUS %>% 
  filter(Key == "18) Please select Not at all for this question.") %>% 
  select(Key, Response, `Participant Private ID`) %>% 
  mutate(IUS_pass = (Response == "Not at all characteristic of me"))

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

SUPPS_score_short <- combined_SUPPS %>% 
  filter(Key == "score") %>% 
  select(`Participant Private ID`, Response, Question) %>% 
  spread(Question, Response)

attention_SUPPS <- combined_SUPPS %>% 
  filter(Key == "5) Please select Agree Strongly for this question.") %>% 
  select(Key, Response, `Participant Private ID`) %>% 
  mutate(SUPPS_pass = (Response == "Agree Strongly"))

# STICSA
STICSA_node_key <- c("questionnaire-mfhw","questionnaire-nuwz","questionnaire-raij","questionnaire-mvz2","questionnaire-gxc8","questionnaire-k93d")
STICSA_files <- paste0(data_dir,"data_exp_172913-v29_", STICSA_node_key,".csv")
STICSA_data_list <- map(STICSA_files, read_csv)

# Ensure all columns have the same data type, e.g., converting all to character
STICSA_data_list <- map(STICSA_data_list, ~mutate_all(.x, as.character)) %>% 
                 map(~head(.x, -1)) # remove the last row

combined_STICSA <- bind_rows(STICSA_data_list) %>% 
  filter(`Response Type` != "info") # remove the information row

STICSA_score_short <- combined_STICSA %>% 
  filter(Question == "Overall impulsiveness") %>% # other three factor is BIS (from template), we forgot to delete them
  select(`Participant Private ID`, Response, Question) %>% 
  spread(Question, Response) %>% 
  mutate(Overall_anxiety = `Overall impulsiveness`) %>%  # We forgot to change the name 
  select(-`Overall impulsiveness`) # remove the column

attention_STICSA <- combined_STICSA %>% 
  filter(Key == "12) Please select A Little for this question") %>% 
  select(Key, Response, `Participant Private ID`) %>% 
  mutate(STICSA_pass = (Response == "A Little"))

# RAS
RAS_node_key <- c("questionnaire-5qtc","questionnaire-x6vn","questionnaire-574d","questionnaire-c427","questionnaire-lnn5","questionnaire-yvcs")
RAS_files <- paste0(data_dir,"data_exp_172913-v29_", RAS_node_key,".csv")
RAS_data_list <- map(RAS_files, read_csv)

# Ensure all columns have the same data type, e.g., converting all to character
RAS_data_list <- map(RAS_data_list, ~mutate_all(.x, as.character)) %>% 
                 map(~head(.x, -1)) # remove the last row

combined_RAS <- bind_rows(RAS_data_list)

attention_RAS <- combined_RAS %>% 
  filter(Key == "value") %>%
  filter(OptionOrder == "Please select this option for this question.|A fair coin flip in which you get £200 if it is heads, £0 if it is tails.") %>% 
  select(Response, `Participant Private ID`) %>% 
  mutate(RAS_pass = (Response == "Please select this option for this question."))

# wrangling RAS data
combined_RAS <- combined_RAS %>% 
  filter(`Response Type` != "info") %>%  # remove the information row
  filter(`Object Number` != 9) %>% # remove attention check
  filter(Key == "quantised") %>% # remove string responses
  mutate(
    Response = as.numeric(Response) - 1, # 1 for gamble, 0 for sure
    gain = 200,
    safe = as.numeric(
      str_extract(OptionOrder, "(?<=£)\\d+(?= for sure)")
    )
  ) %>%
  select(`Participant Private ID`, Response, gain, safe) %>% 
  mutate(ID = as.integer(factor(`Participant Private ID`))) # transform to 1, 2, 3

# combine questionnaire scores
questionnaire_score <- IUS_score_short %>% 
  left_join(SUPPS_score_short, by = "Participant Private ID") %>% 
  left_join(STICSA_score_short, by = "Participant Private ID")

#### Check the attention check and omission rate of main task
# read main data
main_data <- read_csv("raw_data/data_exp_172913-v29/data_exp_172913-v29_task-o8v9.csv") %>% 
             head(-1) # remove the last row

# performance
performance <- main_data %>% 
  group_by(`Participant Private ID`) %>%
  mutate(performance = max(`Store: total_correct`, na.rm = T)) %>%
  mutate(omit = max(`Store: total_no_response`, na.rm = T)) %>%
  filter(`Event Index` == last(`Event Index`)) %>%
  ungroup() %>% 
  select(`Participant Private ID`, performance, omit) %>%
  mutate(omit_pass = (omit <= 60)) %>% 
  mutate(`Participant Private ID` = as.character(`Participant Private ID`)) # for compatible types

# attention
attention_main <- main_data %>% 
  filter(`Component Name` == "Single Number Entry") %>% 
  select(Response, `Participant Private ID`, `Spreadsheet: Win Text`) %>% 
  mutate(main_pass_trial = str_detect(`Spreadsheet: Win Text`, fixed(Response))) %>%
  # some subjects report that they did not realise they can enter a minus sign, so the code above can approve correct answers without a minus sign
  group_by(`Participant Private ID`) %>%
  summarise(count_false = sum(!main_pass_trial)) %>% 
  ungroup() %>%
  mutate(main_pass = (count_false < 5)) %>% 
  mutate(`Participant Private ID` = as.character(`Participant Private ID`)) # for compatible types

# combine inclusion
inclusion <- attention_main %>% 
  left_join(attention_IUS, by = "Participant Private ID") %>% 
  left_join(attention_SUPPS, by = "Participant Private ID") %>% 
  left_join(attention_STICSA, by = "Participant Private ID") %>% 
  left_join(attention_RAS, by = "Participant Private ID") %>% 
  left_join(performance, by = "Participant Private ID") %>%
  select(`Participant Private ID`, main_pass, IUS_pass, SUPPS_pass, STICSA_pass, RAS_pass, omit_pass) %>% 
  mutate(pass = main_pass & IUS_pass & SUPPS_pass & STICSA_pass & RAS_pass & omit_pass) %>%
  select(`Participant Private ID`, pass)

#### Save the inclusion and questionnaire scores
# save inclusion
write_csv(inclusion, paste0(output_dir,"inclusion.csv"))

# save combined questionnaire scores
write_csv(questionnaire_score, paste0(output_dir,"questionnaire_score.csv"))

# save RAS data
write_csv(combined_RAS, paste0(output_dir,"RAS.csv"))