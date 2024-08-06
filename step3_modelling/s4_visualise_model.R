# plot result of modelling

# List of required packages
required_packages <- c("tidyverse",
                       "sjPlot", # plot_model
                       "cowplot", # theme_cowplot
                       "ggsignif", # geom_signif
                       "lattice", # dotplot
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

# Load the model
load("step3_modelling/output/theory_driven_models.rds")

## IU
# fixed effects
IU_fixed_effects <- plot_model(model_IU_3,
                               type = "est",
                               show.values = TRUE,
                               show.p = TRUE,
                               sort.est = TRUE,
                               axis.title = "Coefficients",
                               value.offset = 0.2,
                               transform = NULL, # probit regression shouldn't be transformed by exp
                               title = "Fixed Effects of IU Model") +
  theme_bw() +
  ylim(-0.75, 1)

# random effects
IU_random_effects <- plot_model(model_IU_3,
                                type = "re",
                                sort.est = "V",
                                scales = "free",
                                title = "Random Effects of IU Model",
                                vline.color = "black"
                                ) +
  theme_bw() +
  ylim(0.32, 8.7)

## IM
# fixed effects
IM_fixed_effects <- plot_model(model_IM_3,
                               type = "est",
                               show.values = TRUE,
                               show.p = TRUE,
                               sort.est = TRUE,
                               axis.title = "Coefficients",
                               value.offset = 0.2,
                               transform = NULL,
                               title = "Fixed Effects of IM Model") +
  theme_bw() +
  ylim(-0.75, 1)

# random effects
IM_random_effects <- plot_model(model_IM_3,
                                type = "re",
                                sort.est = "V",
                                scales = "free",
                                title = "Random Effects of IM Model",
                                vline.color = "black"
                                ) +
  theme_bw() +
  ylim(0.32, 8.7)

## Anx
# fixed effects
Anx_fixed_effects <- plot_model(model_Anx_3,
                                type = "est",
                                show.values = TRUE,
                                show.p = TRUE,
                                sort.est = TRUE,
                                axis.title = "Coefficients",
                                value.offset = 0.2,
                                transform = NULL,
                                title = "Fixed Effects of Anx Model") +
  theme_bw() +
  ylim(-0.75, 1)

# random effects
Anx_random_effects <- plot_model(model_Anx_3,
                                 type = "re",
                                 sort.est = "V",
                                 scales = "free",
                                 title = "Random Effects of Anx Model",
                                 vline.color = "black"
                                 ) +
  theme_bw() +
  ylim(0.32, 8.7)

## RA
# fixed effects
RA_fixed_effects <- plot_model(model_RA_2,
                               type = "est",
                               show.values = TRUE,
                               show.p = TRUE,
                               sort.est = TRUE,
                               axis.title = "Coefficients",
                               value.offset = 0.3,
                               transform = NULL,
                               title = "Fixed Effects of RA Model") +
  theme_bw() +
  ylim(-0.75, 1)

# random effects
RA_random_effects <- plot_model(model_RA_2,
                                type = "re",
                                sort.est = "V",
                                scales = "free",
                                title = "Random Effects of RA Model",
                                vline.color = "black"
                                ) +
  theme_bw() +
  ylim(0.32, 8.7)

## Combine fixed effects
fixed_effects <- cowplot::plot_grid(
  IU_fixed_effects,
  IM_fixed_effects,
  Anx_fixed_effects,
  RA_fixed_effects,
  ncol = 2, labels = c("A", "B", "C", "D"))

### Point line plot by traits
## IU
IU_fixed_effects_p <- summary(model_IU_3)$coefficients %>%
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  filter(str_detect(term, "IU")) %>%
  ggplot(aes(x = term, y = Estimate,
             ymin = Estimate + `Std. Error` * -1.96,
             ymax = Estimate + `Std. Error` * 1.96)) +
  geom_pointrange() +
  theme_cowplot() +
  labs(x = "",
    y = "Fixed-effect coefficient")

## IM
IM_fixed_effects_p <- summary(model_IM_3)$coefficients %>%
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  filter(str_detect(term, "IM")) %>%
  ggplot(aes(x = term, y = Estimate,
             ymin = Estimate + `Std. Error` * -1.96,
             ymax = Estimate + `Std. Error` * 1.96)) +
  geom_pointrange() +
  theme_cowplot() +
  annotate(geom = "text", x = 1, y = 0.13, label = "*",  size = 5) +
  annotate(geom = "text", x = 2, y = 0.13, label = "**", size = 5) +
  labs(x = "",
    y = "Fixed-effect coefficient")

## Anx
Anx_fixed_effects_p <- summary(model_Anx_3)$coefficients %>%
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  filter(str_detect(term, "Anx")) %>%
  ggplot(aes(x = term, y = Estimate,
             ymin = Estimate + `Std. Error` * -1.96,
             ymax = Estimate + `Std. Error` * 1.96)) +
  geom_pointrange() +
  theme_cowplot() +
  labs(x = "",
    y = "Fixed-effect coefficient")

## RA
RA_fixed_effects_p <- summary(model_RA_2)$coefficients %>%
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  filter(str_detect(term, "RA")) %>%
  ggplot(aes(x = term, y = Estimate,
             ymin = Estimate + `Std. Error` * -1.96,
             ymax = Estimate + `Std. Error` * 1.96)) +
  geom_pointrange() +
  theme_cowplot() +
  annotate(geom = "text", x = 1, y = 0, label = "*",  size = 5) +
  annotate(geom = "text", x = 2, y = 0.13, label = "**", size = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "",
    y = "Fixed-effect coefficient")

## Combine point line plot
fixed_effects_p <- cowplot::plot_grid(
  IU_fixed_effects_p,
  IM_fixed_effects_p,
  Anx_fixed_effects_p,
  RA_fixed_effects_p,
  align = "h",
  ncol = 2, labels = c("A", "B", "C", "D"))

### Point line plot by terms


## Save
ggsave("step3_modelling/output/fixed_effects.png", fixed_effects, width = 12, height = 12)
ggsave("step3_modelling/output/IU_random_effects.png", IU_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/IM_random_effects.png", IM_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/Anx_random_effects.png", Anx_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/RA_random_effects.png", RA_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/fixed_effects_p.png", fixed_effects_p, width = 10, height = 12)