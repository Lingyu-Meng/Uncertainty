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
    choice = as.factor(choice),
    frame = case_when(
      grepl("Win", frame) ~ -0.5,
      grepl("Lose", frame) ~ 0.5,
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
isSingular(full_model)
summary(full_model) # random effects are highly correlated

full_model_1 <- glmer(
  choice ~ -1 + V + RU + VTU + (-1 + V + RU|ID),
  data = data,
  family = binomial
)

full_model_2 <- glmer(
  choice ~ -1 + V + RU + VTU + (-1 + VTU + RU|ID),
  data = data,
  family = binomial
)
anova(full_model, full_model_1, full_model_2)

frame_model <- glmer(
  choice ~ -1 + V + RU + VTU + (V + RU + VTU):frame + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_1,
  model_2,
  model_3,
  full_model_2,
  frame_model
),
file = "Basic_Models.doc"
)

# IU
model_IU2 <- glmer(
  choice ~ -1 + (V + RU + VTU) * (IIU + PIU) + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU2_model_1 <- glmer(
  choice ~ -1 + V + (RU + VTU) * (IIU + PIU) + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU2_model_2 <- glmer(
  choice ~ -1 + VTU + (V + RU) * (IIU + PIU) + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU2_model_3 <- glmer(
  choice ~ -1 + RU + (V + VTU) * (IIU + PIU) + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU2_model_4 <- glmer(
  choice ~ -1 + V + RU + VTU * (IIU + PIU) + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU2_model_5 <- glmer(
  choice ~ -1 + V + RU * (IIU + PIU) + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU2_model_6 <- glmer(
  choice ~ -1 + V * (IIU + PIU) + RU + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU2,
  IU2_model_1,
  IU2_model_2,
  IU2_model_3,
  IU2_model_4,
  IU2_model_5,
  IU2_model_6,
  full_model_2
  ),
  file = "IU2_Models.doc"
)
# IU x frame
model_IU2_frame <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           (V + RU + VTU):frame:(IIU + PIU) + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_IU2_frame_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           RU:frame:(IIU + PIU) + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU2_frame,
  model_IU2_frame_2,
  frame_model
  ),
  file = "IU2_Frame_Models.doc"
)

# IU1
model_IU1<- glmer(
  choice ~ -1 + (V + RU + VTU) * IU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU1_model_1 <- glmer(
  choice ~ -1 + V + (RU + VTU) * IU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU1_model_2 <- glmer(
  choice ~ -1 + VTU + (V + RU) * IU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU1_model_3 <- glmer(
  choice ~ -1 + RU + (V + VTU) * IU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU1_model_4 <- glmer(
  choice ~ -1 + V + RU + VTU * IU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU1_model_5 <- glmer(
  choice ~ -1 + V + RU * IU + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IU1_model_6 <- glmer(
  choice ~ -1 + V * IU + RU + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU1,
  IU1_model_1,
  IU1_model_2,
  IU1_model_3,
  IU1_model_4,
  IU1_model_5,
  IU1_model_6,
  full_model_2
  ),
  file = "IU1_Models.doc"
)

# IU1 x frame
model_IU1_frame <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           (V + RU + VTU):frame:IU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_IU1_frame_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           RU:frame:IU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IU1_frame,
  model_IU1_frame_2,
  frame_model
  ),
  file = "IU1_Frame_Models.doc"
)

# IM
model_IM <- glmer(
  choice ~ -1 + (V + RU + VTU) * IM + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IM_model_1 <- glmer(
  choice ~ -1 + V + (RU + VTU) * IM + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IM_model_2 <- glmer(
  choice ~ -1 + VTU + (V + RU) * IM + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IM_model_3 <- glmer(
  choice ~ -1 + RU + (V + VTU) * IM + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IM_model_4 <- glmer(
  choice ~ -1 + V + RU + VTU * IM + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IM_model_5 <- glmer(
  choice ~ -1 + V + RU * IM + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

IM_model_6 <- glmer(
  choice ~ -1 + V * IM + RU + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IM,
  IM_model_1,
  IM_model_2,
  IM_model_3,
  IM_model_4,
  IM_model_5,
  IM_model_6,
  full_model_2
  ),
  file = "IM_Models.doc"
)

# IM x frame
model_IM_frame <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           (V + RU + VTU):frame:IM + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_IM_frame_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           RU:frame:IM + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_IM_frame,
  model_IM_frame_2,
  frame_model
  ),
  file = "IM_Frame_Models.doc"
)

# Anx
model_Anx <- glmer(
  choice ~ -1 + (V + RU + VTU) * Anx + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

Anx_model_1 <- glmer(
  choice ~ -1 + V + (RU + VTU) * Anx + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

Anx_model_2 <- glmer(
  choice ~ -1 + VTU + (V + RU) * Anx + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

Anx_model_3 <- glmer(
  choice ~ -1 + RU + (V + VTU) * Anx + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

Anx_model_4 <- glmer(
  choice ~ -1 + V + RU + VTU * Anx + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

Anx_model_5 <- glmer(
  choice ~ -1 + V + RU * Anx + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

Anx_model_6 <- glmer(
  choice ~ -1 + V * Anx + RU + VTU + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_Anx,
  Anx_model_1,
  Anx_model_2,
  Anx_model_3,
  Anx_model_4,
  Anx_model_5,
  Anx_model_6,
  full_model_2
  ),
  file = "Anx_Models.doc"
)

# Anx x frame
model_Anx_frame <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           (V + RU + VTU):frame:Anx + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_Anx_frame_2 <- glmer(
  choice ~ -1 + V + RU + VTU +
           (V + RU + VTU):frame +
           RU:frame:Anx + (-1 + RU + VTU|ID),
  data = data,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

model_summary(list(
  model_Anx_frame,
  model_Anx_frame_2,
  frame_model
  ),
  file = "Anx_Frame_Models.doc"
)
setwd("../../")
# # IU x Anx
# model_Anx_IU <- glmer(
#   choice ~ -1 + V + RU:IU:Anx + VTU + (-1 + V + RU + VTU|ID),
#   data = data,
#   family = binomial,
#   control = glmerControl(optimizer = "bobyqa")
# )
# 
# # all traits
# full_model_traits <- glmer(
#   choice ~ -1 + (V + RU + VTU) * (IU*IM*Anx) + (-1 + V + RU + VTU|ID),
#   data = data,
#   family = binomial,
#   control = glmerControl(optimizer = "bobyqa")
# )
# 
# model_summary(list(
#   model_1,
#   model_2,
#   model_3,
#   full_model,
#   model_IU1, 
#   model_IU2, 
#   RU_model_IU1, 
#   RU_model_IU2, 
#   model_IM, 
#   model_IM_IU, 
#   model_Anx, 
#   model_Anx_IU,
#   model_traits
#   ),
#   file = "GLMM_Models.doc"
# )
