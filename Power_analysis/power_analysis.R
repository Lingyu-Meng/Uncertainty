# Powever analysis for GLMM
# Reference: Kumle, L., Võ, M. L., & Draschkow, D. (2021). Estimating power in (generalized) linear mixed models: an open introduction and tutorial in R. Behav Res. doi:10.3758/s13428-021-01546-0
#            Green, P., & MacLeod, C. J. (2016). SIMR: An R package for power analysis of generalized linear mixed models by simulation. Methods in Ecology and Evolution, 7(4), 493–498. https://doi.org/10.1111/2041-210X.12504
# Priori: Fan, H., Gershman, S. J., & Phelps, E. A. (2023). Trait somatic anxiety is associated with reduced directed exploration and underestimation of uncertainty. Nature Human Behaviour, 7(1), 102–113. https://doi.org/10.1038/s41562-022-01455-y

# We only caculate the power for the fixed effects of RU:IU, as it may be the smallest effect in the model. The hypothetic effect size is 0.06, which is chosen based on the effect size of anxiety in the model (Fan et al., 2023).
# We suppose that the effect size of interaction RU:IU:Loss is 0.06 too.

# List of required packages
required_packages <- c("tidyverse","lme4","simr")

# Check and install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

install_if_missing(required_packages)

if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}

devtools::install_github("DejanDraschkow/mixedpower") # Install the mixedpower package

# Load the required packages
library("tidyverse")
library("mixedpower")
library("lme4")
library("simr")

# According to the tutorial, we are in the scenario 3 as the esixting data does not have the varibale we interested in (IU, IM, Loss)

formula <- Choice ~ V + RU + V/TU + RU:IU + RU:IU:Loss + (V + RU + V/TU|Subject)

# creating artificial data
# 1. create subject IDs  -> Start with 500 subjects
subject_ID <- (1:500)

# 2. create stimuli IDS -> every subject should encounter 240 stimuli
stimuli_ID <- (1:240)

# 3. combine subject IDs and stimui IDs
artificial_data <- expand.grid(Subject = subject_ID, trial = stimuli_ID)

# 4. create vector of Loss (encoded as 1; win as 0)
loss <- c(rep(1, 120), rep(0, 120)) # 120 losses and 120 wins: 12 trials x 5 blocks x 2 conditions

# 5. create vector of V (uniformly distributed between 0.6 and 0.9) (Supplementary Figure 9 from Fan et al. (2023))
V <- runif(240, 0.6, 0.9)

# 6. create vector of RU (uniformly distributed between - 0.2 and - 0.6) (Supplementary Figure 9 from Fan et al. (2023))
RU <- runif(240, -0.6, -0.2)

# 7. create vector of TU (V/TU uniformly distributed between 0.45 and 0.7) (Supplementary Figure 9 from Fan et al. (2023))
`V/TU` <- runif(240, 0.45, 0.7)

# 8. create vector of IU (assumed to be standard normal distributed)
IU <- rnorm(240, 0, 1)

# 9. combine all vectors to the artificial data
artificial_data <- cbind(artificial_data, loss, V, RU, `V/TU`, IU)

# SPECIFY BETA COEFFICIENTS FOR FIXED EFFECTS
fixed_effects <-  c(0, 1.2, 0.4, 1.5, 0.06, 0.06) # order follows the order in formula; intercept as 0
# 0.06 for IU:RU, and IU:RU:Loss; fixed effect of V, RU, V/TU are estimated from the Supplementary Figure 4 from Fan et al. (2023)

# SET RANDOM INTERCEPT VARIANCE
random_variance <- list(1) # random slope are omitted

# ------------------------------------------ #
# create GLMM
artificial_glmer <- makeGlmer(formula = Choice ~ V + RU + `V/TU` + RU:IU + RU:IU:loss +
                                (1 | Subject),
                              family = "binomial", fixef = fixed_effects,
                              VarCorr = random_variance, data = artificial_data)

# lets have a look!
summary(artificial_glmer)

# ------------------------------------------ #
# INFORMATION ABOUT MODEL USED FOR SIMULATION

model <- artificial_glmer # which model do we want to simulate power for?
data <- artificial_data %>% # data used to fit the model
  mutate(`RU:IU` = RU * IU)  %>% 
  mutate(`RU:IU:loss` = RU * IU * loss)
fixed_effects <- c("V", "RU", "V/TU", "RU:IU", "RU:IU:loss") # all fixed effects specified in artificial_glmer
simvar <- "Subject" # which random variable do we want to vary in the simulation?

# ------------------------------------------ #
# SIMULATION PARAMETERS
steps <- c(20,60,100,140,180) # which sample sizes do we want to look at?
critical_value <- 2 # which t/z value do we want to use to test for significance?
n_sim <- 1000 # how many single simulations should be used to estimate power?

# ------------------------------------------ #
# INCLUDE SESOI SIMULATION
SESOI <- c(0, 1.02, 0.34, 1.275, 0.051, 0.051) # specify SESOI (15% smaller betas)
# ------------------------------------------ #
# RUN SIMULATION WITH MIXEDPOWER
power <- mixedpower(model = model, data = data,
                    fixed_effects = fixed_effects,
                    simvar = "Subject", steps = c(200,300,400,600,700),
                    critical_value = 2, n_sim = 1000,
                    SESOI = SESOI)

# ------------------------------------------ #
# PLOT THE RESULTS
multiplotPower(power)
