required_packages <- c("tidyverse", "lme4", "bruceR")

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

data <- read_csv("step3_modelling/output/data.csv") %>% 
  mutate(
    ID = as.factor(ID),
    choice = 2 - choice, # V = Q(1) - Q(2); encode arm 1 as 1 and arm 2 as 0, therefore, positive w1 indicate a choice of arm 1
    context = case_when(
      grepl("Win", context) ~ -0.5,
      grepl("Lose", context) ~ 0.5,
    )
  )
setwd("step3_modelling/output/")

# trait free
context_model <- glmer(
  choice ~ -1 + V + RU + VTU + (V + RU + VTU):context + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# all traits model
full_model <- glmer(
  choice ~ -1 + V + RU + VTU + (V + RU + VTU):context +
           V  :(IU + IM + Anx + RA):(1+context) +
           RU :(IU + IM + Anx + RA):(1+context) +
           VTU:(IU + IM + Anx + RA):(1+context) +
           (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# all traits without interaction between traits and context
no_context_interaction <- glmer(
  choice ~ -1 + V + RU + VTU + (V + RU + VTU):context +
           V  :(IU + IM + Anx + RA) +
           RU :(IU + IM + Anx + RA) +
           VTU:(IU + IM + Anx + RA) +
           (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# all traits with interaction between traits and context only
only_context_interaction <- glmer(
  choice ~ -1 + V + RU + VTU + (V + RU + VTU):context +
           V  :(IU + IM + Anx + RA):context +
           RU :(IU + IM + Anx + RA):context +
           VTU:(IU + IM + Anx + RA):context +
           (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  context_model,
  full_model,
  no_context_interaction,
  only_context_interaction
  ),
file = "All_Traits.doc")

save(full_model,
     no_context_interaction,
     only_context_interaction,
     file = "All_Traits.RData")

setwd("../../")