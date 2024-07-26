# Manipulation check
# This script is used to check the manipulation of the experiment' conditions without Kalman filter
# We will use a basic model (V) which will have slope and intercept for each condition

required_packages <- c("tidyverse","lme4","sjPlot")

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

# Load the data
data <- read_csv("step3_modelling/output/data.csv") %>% 
  mutate(ID = as.factor(ID),
         condition = paste0(context, gsub("\\**", "", left), gsub("\\**", "", right)), # combine it for convenience
         condition = factor(condition),
         choice = 2 - choice) # V = Q(1) - Q(2); encode arm 1 as 1 and arm 2 as 0, therefore, positive w1 indicate a choice of arm 1

# Modelling
condition_model <- glmer(choice ~ -1 + condition + condition:V + (-1 + V|ID),
                       data = data, family = binomial(link = "probit"),
                       control = glmerControl(optimizer = "bobyqa"))

# Visualisation
risk_ratios <- plot_model(condition_model) +
  ylim(0.5, 3) 
coefficients <- summary(condition_model)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "condition") %>% 
  ggplot(aes(x = condition, y = Estimate,
             ymin = Estimate + `Std. Error` * -1.96,
             ymax = Estimate + `Std. Error` * 1.96)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Slopes and intercepts for each condition",
       x = "Condition",
       y = "Estimate") +
  coord_flip()

# Save
ggsave("step3_modelling/output/conditions_coefficients.png", coefficients, width = 5, height = 5)
ggsave("step3_modelling/output/conditions_risk_ratios.png", risk_ratios, width = 5, height = 5)