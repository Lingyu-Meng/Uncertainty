# Power analysis for GLMM
# Notice: Calculation intensive script, may need a whole day
# We only calculate the power for the fixed effects of RU:IU, as it may be the 
# smallest effect in the model. The hypothetical max effect size is 0.2, which is
# chosen based on the effect size of anxiety in the model (Fan et al., 2023).

# We omitted interaction RU:IU:Loss this time.

# Reference: Kumle, L., Võ, M. L., & Draschkow, D. (2021). Estimating power in (generalized) linear mixed models: an open introduction and tutorial in R. Behav Res. doi:10.3758/s13428-021-01546-0
#            Green, P., & MacLeod, C. J. (2016). SIMR: An R package for power analysis of generalized linear mixed models by simulation. Methods in Ecology and Evolution, 7(4), 493–498. https://doi.org/10.1111/2041-210X.12504

# Priori: Fan, H., Gershman, S. J., & Phelps, E. A. (2023). Trait somatic anxiety is associated with reduced directed exploration and underestimation of uncertainty. Nature Human Behaviour, 7(1), 102–113. https://doi.org/10.1038/s41562-022-01455-y

# List of required packages
required_packages <- c("tidyverse","lme4","simr","cowplot","furrr")

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

# According to the tutorial, we are in the scenario 3 as the existing data does 
# not have the variable we interested in (IU, IM, Loss)

# formula <- Choice ~ V + RU + V/TU + RU:IU + RU:IU:Loss + (V + RU + V/TU|Subject)

# creating artificial data
# 1. create subject IDs  -> Start with 500 subjects
subject_ID <- (1:500)

# 2. create stimuli IDS -> every subject should encounter 240 stimuli
stimuli_ID <- (1:240)

# 3. combine subject IDs and stimuli IDs
artificial_data <- expand.grid(Subject = subject_ID, trial = stimuli_ID)

# 4. create vector of Loss (encoded as 1; win as 0)
loss <- c(rep(1, 120), rep(0, 120)) 
# 120 losses and 120 wins: 12 trials x 5 blocks x 2 conditions

# 5. create vector of V (uniformly distributed between 0.6 and 0.9) 
# (Supplementary Figure 9 from Fan et al. (2023))
V <- runif(240, 0.6, 0.9)

# 6. create vector of RU (uniformly distributed between - 0.2 and - 0.6) 
# (Supplementary Figure 9 from Fan et al. (2023))
RU <- runif(240, -0.6, -0.2)

# 7. create vector of TU (V/TU uniformly distributed between 0.45 and 0.7) 
# (Supplementary Figure 9 from Fan et al. (2023))
`V/TU` <- runif(240, 0.45, 0.7)

# 8. create vector of IU (assumed to be standard normal distributed)
IU <- rnorm(500, 0, 1)

# 9. combine all vectors to the artificial data
artificial_data <- cbind(artificial_data, loss, V, RU, `V/TU`, IU)

# Define a range of effect sizes
effect_sizes <- seq(0.04, 0.2, by = 0.02)
# SPECIFY BETA COEFFICIENTS FOR FIXED EFFECTS
fixed_effects <-  c(0, 1.2, 0.4, 1.5, 0.2) # order follows the order in formula; intercept as 0
# 0.2 for V:RU; fixed effect of V, RU, V/TU are estimated from the Supplementary 
# Figure 4 from Fan et al. (2023)

# SET RANDOM INTERCEPT VARIANCE
random_variance <- list(1) # random slope are omitted

# ------------------------------------------ #
# create GLMM
artificial_glmer <- makeGlmer(formula = Choice ~ V + RU + `V/TU` + RU:IU +
                                (1 | Subject),
                              family = "binomial", fixef = fixed_effects,
                              VarCorr = random_variance, data = artificial_data)

# lets have a look!
summary(artificial_glmer)

# Store power results
power_results <- data.frame(effect_size = effect_sizes,
                            power = NA, lower = NA, upper = NA)

# Function to perform power analysis for a given effect size
power_analysis_function <- function(effect_size, model, predictor) {
  model_copy <- model
  fixef(model_copy)[predictor] <- effect_size
  power_sim <- powerSim(model_copy, test = fixed(predictor),
                        nsim = 1000)  # it is suggested to use 1000 simulations (Kumle et al., 2021)
  summary_sim <- summary(power_sim)
  return(data.frame(
    effect_size = effect_size,
    power = summary_sim$mean,
    lower_ci = summary_sim$lower,
    upper_ci = summary_sim$upper
  ))
}

# Set up parallel processing
plan(multisession)

# Perform power analysis in parallel
power_results <- future_map_dfr(effect_sizes, ~power_analysis_function(.x, artificial_glmer, "RU:IU"))

# Plot the power results
power_500 <- ggplot(power_results, aes(x = effect_size, y = power)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(
    title = "Power Analysis on RU:IU for 500 sample size",
    x = "Effect Size",
    y = "Power",
    caption = "95% CI are shown as shaded regions"
  ) +
  xlim(0.04, 0.2) +
  geom_abline(aes(intercept = 0.8, slope = 0), linetype = "dashed") +
  theme_cowplot()

ggsave("power_500.png")