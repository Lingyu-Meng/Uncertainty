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
                               axis.title = "Odds ratio",
                               title = "Fixed Effects of IU Model") +
  theme_bw() +
  ylim(0.5, 2.7)

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
                               axis.title = "Odds ratio",
                               title = "Fixed Effects of IM Model") +
  theme_bw() +
  ylim(0.5, 2.7)

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
                                axis.title = "Odds ratio",
                                title = "Fixed Effects of Anx Model") +
  theme_bw() +
  ylim(0.5, 2.7)

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
                               axis.title = "Odds ratio",
                               title = "Fixed Effects of RA Model") +
  theme_bw() +
  ylim(0.5, 2.7)

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

## Save
ggsave("step3_modelling/output/fixed_effects.png", fixed_effects, width = 12, height = 12)
ggsave("step3_modelling/output/IU_random_effects.png", IU_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/IM_random_effects.png", IM_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/Anx_random_effects.png", Anx_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/RA_random_effects.png", RA_random_effects, width = 15, height = 15)