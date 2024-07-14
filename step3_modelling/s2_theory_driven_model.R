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
model_1 <- glmer(
  choice ~ -1 + V + (-1 + V|ID),
  data = data,
  family = binomial
)

model_2 <- glmer(
  choice ~ -1 + VTU + (-1 + VTU|ID),
  data = data,
  family = binomial
)

model_3 <- glmer(
  choice ~ -1 + V + RU + (-1 + V + RU|ID),
  data = data,
  family = binomial
)

full_model <- glmer(
  choice ~ -1 + V + RU + VTU + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial
)

context_model <- glmer(
  choice ~ -1 + V + RU + VTU + (V + RU + VTU):context + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_1,
  model_2,
  model_3,
  full_model,
  context_model
),
file = "Basic_Models.doc"
)

## theory-derived models IU2
# H1: IIU, PIU affect RU
model_IU2_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:(IIU + PIU) + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H1: IIU, PIU affect V, RU, VTU
model_IU2_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
           V:(IIU + PIU) +
           RU:(IIU + PIU) +
           VTU:(IIU + PIU) +
           (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H3: IIU affect RU more under lose context & H1
model_IU2_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:(IIU + PIU) + RU:IIU:context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H3: IIU and PIU affect V, RU, VTU more under lose context & H1
model_IU2_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
           V:(IIU + PIU) +
           RU:(IIU + PIU) +
           VTU:(IIU + PIU) +
           V:(IIU + PIU):context +
           RU:(IIU + PIU):context + 
           VTU:(IIU + PIU):context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H1 & H3 with considering context
model_IU2_5 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:(IIU + PIU) + RU:IIU:context +
          (V + RU + VTU):context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU2_1,
  model_IU2_2,
  model_IU2_3,
  model_IU2_4,
  model_IU2_5,
  context_model
  ),
file = "theory_IU2_Models.doc"
)

## theory-driven models IU
# H1b: IU affect RU
model_IU_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:IU + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H1b: IU affect V, RU, VTU
model_IU_2 <- glmer(
  choice ~ -1 + V + RU + VTU + 
           V:IU +
           RU:IU +
           VTU:IU +
           (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H3b: IU x context affect RU & H1b
model_IU_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:IU + RU:IU:context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H3b: IU x context affect V RU VTU & H1b
model_IU_4 <- glmer(
  choice ~ -1 + V + RU + VTU + 
           V:IU +
           RU:IU +
           VTU:IU +
           V:IU:context +
           RU:IU:context +
           VTU:IU:context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H1 & H3 with considering context
model_IU_5 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:IU + RU:IU:context +
          (V + RU + VTU):context + 
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU_1,
  model_IU_2,
  model_IU_3,
  model_IU_4,
  model_IU_5,
  context_model
),
file = "theory_IU_Models.doc"
)

# Theory-driven models IM
# H2: IM affect V/TU
model_IM_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
           VTU:IM + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H2: IM affect V, RU, V/TU
model_IM_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
           V:IM +
           RU:IM +
           VTU:IM +
           (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H4: IM x context affect V/TU & H2
model_IM_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
           VTU:IM + VTU:IM:context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H4: IM x context affect V, RU, V/TU & H2
model_IM_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
           V:IM +
           RU:IM +
           VTU:IM +
           V:IM:context +
           RU:IM:context +
           VTU:IM:context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# H2 & H4 with considering context
model_IM_5 <- glmer(
  choice ~ -1 + V + RU + VTU +
           VTU:IM + VTU:IM:context +
          (V + RU + VTU):context + 
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
model_summary(list(
  model_IM_1,
  model_IM_2,
  model_IM_3,
  model_IM_4,
  model_IM_5,
  context_model
),
file = "theory_IM_Models.doc"
)

# theory-driven models Anx
# Anx affect RU
model_Anx_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:Anx + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# Anx affect V, RU, VTU
model_Anx_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
           V:Anx +
           RU:Anx +
           VTU:Anx +
           (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# Anx x context affect RU & Anx affect RU
model_Anx_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:Anx + RU:Anx:context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# Anx x context & Anx affect V, RU, VTU
model_Anx_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
           V:Anx +
           RU:Anx +
           VTU:Anx +
           V:Anx:context +
           RU:Anx:context +
           VTU:Anx:context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
# with context
model_Anx_5 <- glmer(
  choice ~ -1 + V + RU + VTU +
           RU:Anx + RU:Anx:context +
          (V + RU + VTU):context +
          (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
model_summary(list(
  model_Anx_1,
  model_Anx_2,
  model_Anx_3,
  model_Anx_4,
  model_Anx_5,
  context_model
),
file = "theory_Anx_Models.doc"
)
rm(data) # remove data from memory
# save all models
save.image(file = 'theory_driven_models.rds')
setwd("../../")