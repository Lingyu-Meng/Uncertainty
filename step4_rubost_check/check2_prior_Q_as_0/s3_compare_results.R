# compare the models with the 0 initial data and 4 initial data

# List of required packages
required_packages <- c("tidyverse",
                       "cowplot", # theme_cowplot
                       "ggsignif", # geom_signif
                       "bruceR", # HLM_summary
                       "lme4") # lmer

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

# Load the data with initial 0
load("step4_rubost_check/check2_prior_Q_as_0/output/theory_driven_models.rds")

# store the models (as we used the same name)
context_prior_0 <- context_model
# IU_prior_0 <- IU_model # we will not compare the IU models as there is no difference in terms of significant predictors
IM_prior_0 <- model_IM_3 # we selected the model 3
Anx_prior_0 <- model_Anx_3
RA_prior_0 <- model_RA_2

# Load the data with initial 4
load("step3_modelling/output/theory_driven_models.rds")

# store the models (as we used the same name)
context_prior_4 <- context_model
# IU_prior_0 <- IU_model # we will not compare the IU models as there is no difference in terms of significant predictors
IM_prior_4 <- model_IM_3 # we selected the model 5
Anx_prior_4 <- model_Anx_3
RA_prior_4 <- model_RA_2

# compare the models
model_summary(context_prior_0)
model_summary(IM_prior_0)
model_summary(Anx_prior_0)
model_summary(RA_prior_0) # high VIF in V:context and V:RA:context

model_summary(context_prior_4) # high VIF in V:context and VTU:context
model_summary(IM_prior_4)      # high VIF in V:context and VTU:context
model_summary(Anx_prior_4)     # high VIF in V:context and VTU:context
model_summary(RA_prior_4)      # high VIF in V:context, VTU:context, V:RA:context, and VTU:RA:context

model_summary(list(context_prior_0, context_prior_4),
              modify.head = c("Prior 0", "Prior 4"))
model_summary(list(IM_prior_0, IM_prior_4),
              modify.head = c("Prior 0", "Prior 4"))
model_summary(list(Anx_prior_0, Anx_prior_4),
              modify.head = c("Prior 0", "Prior 4"))
model_summary(list(RA_prior_0, RA_prior_4),
              modify.head = c("Prior 0", "Prior 4"))

## We will using z-test to test the difference between the coefficients 
# Context
coef_context_1 <- coef(summary(context_prior_0)) %>% 
  as.data.frame() %>% 
  transmute(beta_p0 = Estimate, SE_p0 = `Std. Error`)
coef_context_4 <- coef(summary(context_prior_4)) %>%
  as.data.frame() %>%
  transmute(beta_p4 = Estimate, SE_p4 = `Std. Error`)
coef_context <- merge(coef_context_1, coef_context_4, by = "row.names") %>% 
  mutate(z_stat = (beta_p0 - beta_p4) / sqrt(SE_p0^2 + SE_p4^2), # Calculate the t-statistic
         p = 2 * pnorm(-abs(z_stat)), # Calculate the p value by assuming a two-tailed test and using the normal distribution
         sig = ifelse(p < 0.05, TRUE, FALSE)) # Add a significance indicator (Ture) if the p value is less than 0.05

# IM
coef_IM_1 <- coef(summary(IM_prior_0)) %>% 
  as.data.frame() %>% 
  transmute(beta_p0 = Estimate, SE_p0 = `Std. Error`)
coef_IM_4 <- coef(summary(IM_prior_4)) %>%
  as.data.frame() %>%
  transmute(beta_p4 = Estimate, SE_p4 = `Std. Error`)
coef_IM <- merge(coef_IM_1, coef_IM_4, by = "row.names") %>%
  mutate(z_stat = (beta_p0 - beta_p4) / sqrt(SE_p0^2 + SE_p4^2), # Calculate the t-statistic
         p = 2 * pnorm(-abs(z_stat)), # Calculate the p value by assuming a two-tailed test and using the normal distribution
         sig = ifelse(p < 0.05, TRUE, FALSE)) # Add a significance indicator (Ture) if the p value is less than 0.05
# Anx
coef_Anx_1 <- coef(summary(Anx_prior_0)) %>% 
  as.data.frame() %>% 
  transmute(beta_p0 = Estimate, SE_p0 = `Std. Error`)
coef_Anx_4 <- coef(summary(Anx_prior_4)) %>%
  as.data.frame() %>%
  transmute(beta_p4 = Estimate, SE_p4 = `Std. Error`)
coef_Anx <- merge(coef_Anx_1, coef_Anx_4, by = "row.names") %>%
  mutate(z_stat = (beta_p0 - beta_p4) / sqrt(SE_p0^2 + SE_p4^2), # Calculate the t-statistic
         p = 2 * pnorm(-abs(z_stat)), # Calculate the p value by assuming a two-tailed test and using the normal distribution
         sig = ifelse(p < 0.05, TRUE, FALSE)) # Add a significance indicator (Ture) if the p value is less than 0.05
# RA
coef_RA_1 <- coef(summary(RA_prior_0)) %>% 
  as.data.frame() %>% 
  transmute(beta_p0 = Estimate, SE_p0 = `Std. Error`)
coef_RA_4 <- coef(summary(RA_prior_4)) %>%
  as.data.frame() %>%
  transmute(beta_p4 = Estimate, SE_p4 = `Std. Error`)
coef_RA <- merge(coef_RA_1, coef_RA_4, by = "row.names") %>%
  mutate(z_stat = (beta_p0 - beta_p4) / sqrt(SE_p0^2 + SE_p4^2), # Calculate the t-statistic
         p = 2 * pnorm(-abs(z_stat)), # Calculate the p value by assuming a two-tailed test and using the normal distribution
         sig = ifelse(p < 0.05, TRUE, FALSE)) # Add a significance indicator (Ture) if the p value is less than 0.05

# Visualize the results
f1 <- coef_context %>%
  pivot_longer(
    cols = c("beta_p0", "SE_p0", "beta_p4", "SE_p4"),        # Specify the columns to pivot
    names_to = c(".value", "condition"), # Split the column names into .value and condition
    names_sep = "_"             # Specify the separator that splits the names
  ) %>% 
  ggplot(aes(x = reorder(Row.names, beta), y = beta, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = beta - 1.96 * SE, ymax = beta + 1.96 * SE), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Predictors", y = "coefficients", title = "Context") +
  geom_segment(data = coef_context,
             aes(x = Row.names, xend = Row.names,
                 y = beta_p0,   yend = beta_p4,
                 linetype = sig),
             colour = "black", linewidth = 1)

f2 <- coef_IM %>%
  pivot_longer(
    cols = c("beta_p0", "SE_p0", "beta_p4", "SE_p4"),        # Specify the columns to pivot
    names_to = c(".value", "condition"), # Split the column names into .value and condition
    names_sep = "_"             # Specify the separator that splits the names
  ) %>% 
  ggplot(aes(x = reorder(Row.names, beta), y = beta, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = beta - 1.96 * SE, ymax = beta + 1.96 * SE), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Predictors", y = "coefficients", title = "IM") +
  geom_segment(data = coef_IM,
               aes(x = Row.names, xend = Row.names,
                   y = beta_p0,   yend = beta_p4,
                   linetype = sig),
               colour = "black", linewidth = 1)

f3 <- coef_Anx %>%
  pivot_longer(
    cols = c("beta_p0", "SE_p0", "beta_p4", "SE_p4"),        # Specify the columns to pivot
    names_to = c(".value", "condition"), # Split the column names into .value and condition
    names_sep = "_"             # Specify the separator that splits the names
  ) %>% 
  ggplot(aes(x = reorder(Row.names, beta), y = beta, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = beta - 1.96 * SE, ymax = beta + 1.96 * SE), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Predictors", y = "coefficients", title = "Anx") +
  geom_segment(data = coef_Anx,
               aes(x = Row.names, xend = Row.names,
                   y = beta_p0,   yend = beta_p4,
                   linetype = sig),
               colour = "black", linewidth = 1)

f4 <- coef_RA %>%
  pivot_longer(
    cols = c("beta_p0", "SE_p0", "beta_p4", "SE_p4"),        # Specify the columns to pivot
    names_to = c(".value", "condition"), # Split the column names into .value and condition
    names_sep = "_"             # Specify the separator that splits the names
  ) %>% 
  ggplot(aes(x = reorder(Row.names, beta), y = beta, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = beta - 1.96 * SE, ymax = beta + 1.96 * SE), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Predictors", y = "coefficients", title = "RA") +
  geom_segment(data = coef_RA,
               aes(x = Row.names, xend = Row.names,
                   y = beta_p0,   yend = beta_p4,
                   linetype = sig),
               colour = "black", linewidth = 1)

Fig <- plot_grid(f1 + theme(legend.position="none"),
                 f2 + theme(legend.position="none"),
                 f3 + theme(legend.position="none"), 
                 f4 + theme(legend.position="none"),
                 ncol = 2, nrow = 2)
# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  f2 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
# add the legend
FIG <- plot_grid(Fig, legend, rel_widths = c(3, .4))

# remove regressor which is irrelevant with traits
f5 <- coef_IM %>%
  pivot_longer(
    cols = c("beta_p0", "SE_p0", "beta_p4", "SE_p4"),        # Specify the columns to pivot
    names_to = c(".value", "condition"), # Split the column names into .value and condition
    names_sep = "_"             # Specify the separator that splits the names
  ) %>% 
  filter(Row.names == "VTU:IM" | Row.names == "VTU:IM:context") %>%
  ggplot(aes(x = reorder(Row.names, beta), y = beta, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = beta - 1.96 * SE, ymax = beta + 1.96 * SE), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Predictors", y = "coefficients", title = "IM") +
  geom_segment(data = coef_IM,
               aes(x = Row.names, xend = Row.names,
                   y = beta_p0,   yend = beta_p4,
                   linetype = sig),
               colour = "black", linewidth = 1) +
  ylim(-0.15, 0.15)

f6 <- coef_Anx %>%
  pivot_longer(
    cols = c("beta_p0", "SE_p0", "beta_p4", "SE_p4"),        # Specify the columns to pivot
    names_to = c(".value", "condition"), # Split the column names into .value and condition
    names_sep = "_"             # Specify the separator that splits the names
  ) %>% 
  filter(Row.names == "RU:Anx" | Row.names == "RU:Anx:context") %>%
  ggplot(aes(x = reorder(Row.names, beta), y = beta, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = beta - 1.96 * SE, ymax = beta + 1.96 * SE), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Predictors", y = "coefficients", title = "Anx") +
  geom_segment(data = coef_Anx,
               aes(x = Row.names, xend = Row.names,
                   y = beta_p0,   yend = beta_p4,
                   linetype = sig),
               colour = "black", linewidth = 1) +
  ylim(-0.15, 0.15)

f7 <- coef_RA %>%
  pivot_longer(
    cols = c("beta_p0", "SE_p0", "beta_p4", "SE_p4"),        # Specify the columns to pivot
    names_to = c(".value", "condition"), # Split the column names into .value and condition
    names_sep = "_"             # Specify the separator that splits the names
  ) %>% 
  filter(Row.names == "RU:RA" | Row.names == "RU:RA:context" |
         Row.names == "V:RA"  | Row.names == "V:RA:context"  |
         Row.names == "VTU:RA" | Row.names == "VTU:RA:context" ) %>%
  ggplot(aes(x = reorder(Row.names, beta), y = beta, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  geom_errorbar(aes(ymin = beta - 1.96 * SE, ymax = beta + 1.96 * SE), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Predictors", y = "coefficients", title = "RA") +
  geom_segment(data = coef_RA,
               aes(x = Row.names, xend = Row.names,
                   y = beta_p0,   yend = beta_p4,
                   linetype = sig),
               colour = "black", linewidth = 1) +
  ylim(-0.4, 0.3)

Fig2 <- plot_grid(f5 + theme(legend.position="none"),
                  f6 + theme(legend.position="none"),
                  f7 + theme(legend.position="none"),
                  ncol = 1)
FIG2 <- plot_grid(Fig2, legend, rel_widths = c(3, .4))
# save the plot
ggsave("step4_rubost_check/check2_prior_Q_as_0/output/compare_coef.png", FIG, width = 10, height = 10, units = "in", dpi = 300)
ggsave("step4_rubost_check/check2_prior_Q_as_0/output/compare_coef_reduced.png", FIG2, width = 4, height = 12, units = "in", dpi = 300)