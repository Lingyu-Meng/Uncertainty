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
    ),
    V = scale(V),    # standardize V, RU, VTU to have beta for every predictor
    RU = scale(RU),
    VTU = scale(VTU)
  )
setwd("step3_modelling/output/")

# trait free
model_1 <- glmer(
  choice ~ -1 + V + (-1 + V|ID),
  data = data,
  family = binomial(link = "probit")
)

model_2 <- glmer(
  choice ~ -1 + VTU + (-1 + VTU|ID),
  data = data,
  family = binomial(link = "probit")
)

model_3 <- glmer(
  choice ~ -1 + V + RU + (-1 + V + RU|ID),
  data = data,
  family = binomial(link = "probit")
)

full_model <- glmer(
  choice ~ -1 + V + RU + VTU + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit")
)

context_model <- glmer(
  choice ~ -1 + V + RU + VTU + (V + RU + VTU):context + (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
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
# H1: IIU, PIU affect V
model_IU2_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:(IIU + PIU) + V:(IIU + PIU):context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H1: IIU, PIU affect RU
model_IU2_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    RU:(IIU + PIU) + RU:(IIU + PIU):context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H3: IIU and PIU affect VTU
model_IU2_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    VTU:(IIU + PIU) + VTU:(IIU + PIU):context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H3: IIU and PIU affect V, RU, VTU
model_IU2_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:(IIU + PIU) + V:(IIU + PIU):context +
    RU:(IIU + PIU) + RU:(IIU + PIU):context +
    VTU:(IIU + PIU) + VTU:(IIU + PIU):context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU2_1,
  model_IU2_2,
  model_IU2_3,
  model_IU2_4,
  context_model
  ),
file = "theory_IU2_Models.doc"
)

## theory-driven models IU
# H1b: IU affect V
model_IU_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:IU + V:IU:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H1b: IU affect RU
model_IU_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    RU:IU + RU:IU:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H3b: IU affect VTU
model_IU_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    VTU:IU + VTU:IU:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H3b: IU affect V RU VTU
model_IU_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:IU + V:IU:context +
    RU:IU + RU:IU:context +
    VTU:IU + VTU:IU:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU_1,
  model_IU_2,
  model_IU_3,
  model_IU_4,
  context_model
),
file = "theory_IU_Models.doc"
)

# Theory-driven models IM
# H2: IM affect V
model_IM_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:IM + V:IM:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H2: IM affect RU
model_IM_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    RU:IM + RU:IM:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H2: IM affect V/TU
model_IM_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    VTU:IM + VTU:IM:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# H4: IM affect V, RU, V/TU
model_IM_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:IM + V:IM:context +
    RU:IM + RU:IM:context +
    VTU:IM + VTU:IM:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)


model_summary(list(
  model_IM_1,
  model_IM_2,
  model_IM_3,
  model_IM_4,
  context_model
),
file = "theory_IM_Models.doc"
)

# theory-driven models Anx
# Anx affect V
model_Anx_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:Anx + V:Anx:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# Anx affect RU
model_Anx_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    RU:Anx + RU:Anx:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# Anx affect VTU
model_Anx_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    VTU:Anx + VTU:Anx:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# Anx affect V, RU, VTU
model_Anx_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:Anx + V:Anx:context +
    RU:Anx + RU:Anx:context +
    VTU:Anx + VTU:Anx:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_Anx_1,
  model_Anx_2,
  model_Anx_3,
  model_Anx_4,
  context_model
),
file = "theory_Anx_Models.doc"
)

# theory-driven models RA
# RA affect V
model_RA_1 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:RA + V:RA:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# RA affect RU
model_RA_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    RU:RA + RU:RA:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# RA affect VTU
model_RA_3 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    VTU:RA + VTU:RA:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
# RA affect V, RU, VTU
model_RA_4 <- glmer(
  choice ~ -1 + V + RU + VTU +
    (V + RU + VTU):context +
    V:RA + V:RA:context +
    RU:RA + RU:RA:context +
    VTU:RA + VTU:RA:context +
    (-1 + V + RU + VTU|ID),
  data = data,
  family = binomial(link = "probit"),
  control = glmerControl(optimizer = "bobyqa")
)
model_summary(list(
  model_RA_1,
  model_RA_2,
  model_RA_3,
  model_RA_4,
  context_model
),
file = "theory_RA_Models.doc"
)

rm(data) # remove data from memory
# save all models
save.image(file = 'theory_driven_models.rds', compress = "xz")
setwd("../../")