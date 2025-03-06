# plot result of modelling

# List of required packages
required_packages <- c("tidyverse",
                       "sjPlot", # plot_model
                       "cowplot", # theme_cowplot
                       "ggsignif", # geom_signif
                       "lattice", # dotplot
                       "lme4", # lmer
                       "ggrain", # geom_rain
                       "png")

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
    y = "Fixed-effect coefficient") +
  ylim(-0.075, 0.15)

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
  annotate(geom = "text", x = 1, y = 0.135, label = "*",  size = 5) +
  annotate(geom = "text", x = 2, y = 0.135, label = "**", size = 5) +
  labs(x = "",
    y = "Fixed-effect coefficient") +
  ylim(-0.075, 0.15)

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
  theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
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

### distribution of three strategies
strategies <- coef(full_model)$ID %>% # Get the real slope for everyone
  as.data.frame() %>%
  gather(key = "Strategy", value = "value") 

strategies_den <- strategies %>%
  ggplot(aes(x = value, fill = Strategy)) +
  geom_density(alpha = 0.5) +
  theme_cowplot() +
  facet_wrap(~Strategy, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Slope",
       y = "Density")

### visualise trait free model
contexts_coeff <- t(coef(summary(context_model))) %>%
  as.data.frame() %>%
  slice(1) %>% 
  transmute(`Value-dependent random exploration in Loss` = V + 0.5 * `V:context`,
         `Value-dependent random exploration in Win`  = V - 0.5 * `V:context`,
         `Directed exploration in Loss` = RU + 0.5 * `RU:context`,
         `Directed exploration in Win`  = RU - 0.5 * `RU:context`,
         `Uncertainty-dependent random exploration in Loss` = VTU + 0.5 * `VTU:context`,
         `Uncertainty-dependent random exploration in Win`  = VTU - 0.5 * `VTU:context`
         ) %>%
  gather("Term", "Estimate") %>%
  separate(Term, c("Strategy", "Context"), sep = " in ") %>%
  ggplot(aes(x = Strategy, y = Estimate, fill = Context)) +
  geom_bar(position="dodge",stat="identity") +
  geom_segment(y = -0.65, yend = -0.65, x = 0.7, xend = 1.3) +
  geom_segment(y = 0.98, yend = 0.98, x = 1.7, xend = 2.3) +
  geom_segment(y = -0.36, yend = -0.36, x = 2.7, xend = 3.3) +
  annotate(geom = "text", x = 1, y = -0.7, label = "N.S.") +
  annotate(geom = "text", x = 2, y = 1, label = "**") +
  annotate(geom = "text", x = 3, y = -0.41, label = "**") +
  theme_cowplot() +
  theme(axis.text.x = element_text(size = 10))

psychometric_cur <- readPNG("step3_modelling/output/conditions_psychometric_curve.png")
psychometric_cur_gg <- ggdraw() + draw_image(psychometric_cur, x = 0, y = 0, width = 1, height = 1)

conext_results <- cowplot::plot_grid(
  contexts_coeff,
  psychometric_cur_gg,
  rel_widths = c(1, 0.5),
  ncol = 2, labels = c("A", "B"))

#### context model with data points (random effects)
contexts_raincloud <- coef(context_model)$ID %>% # Get the real slope for everyone
  as.data.frame() %>%
  transmute(`Uncertainty-independent random exploration in Loss` = V + 0.5 * `V:context`,
            `Uncertainty-independent random exploration in Win`  = V - 0.5 * `V:context`,
            `Directed exploration in Loss` = RU + 0.5 * `RU:context`,
            `Directed exploration in Win`  = RU - 0.5 * `RU:context`,
            `Uncertainty-dependent random exploration in Loss` = VTU + 0.5 * `VTU:context`,
            `Uncertainty-dependent random exploration in Win`  = VTU - 0.5 * `VTU:context`
  ) %>%
  gather(key = "Strategy", value = "Coefficient") %>%
  mutate(Context = ifelse(str_detect(Strategy, "Loss"), "Loss", "Win"),
         Strategy = str_remove(Strategy, " in Loss| in Win")) %>%
  ggplot(aes(x = Strategy, y = Coefficient, fill = Context)) +
  # drow raincloud and aviod overlap boxplot
  geom_rain(alpha = 0.5, point.args = list(alpha = .2)) +
  theme_cowplot() +
  geom_segment(y = 0, yend = 0, x = 0.9, xend = 1.3) +
  geom_segment(y = 2.5, yend = 2.5, x = 1.9, xend = 2.3) +
  geom_segment(y = 0.6, yend = 0.6, x = 2.9, xend = 3.3) +
  annotate(geom = "text", x = 1.1, y = 0.1, label = "N.S.") +
  annotate(geom = "text", x = 2.1, y = 2.6, label = "**") +
  annotate(geom = "text", x = 3.1, y = 0.7, label = "**") +
  theme(axis.text.x = element_text(size = 10))

## Save
ggsave("step3_modelling/output/fixed_effects.png", fixed_effects, width = 12, height = 12)
ggsave("step3_modelling/output/IU_random_effects.png", IU_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/IM_random_effects.png", IM_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/Anx_random_effects.png", Anx_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/RA_random_effects.png", RA_random_effects, width = 15, height = 15)
ggsave("step3_modelling/output/fixed_effects_p.png", fixed_effects_p, width = 10, height = 12)
ggsave("step3_modelling/output/strategies_den.png", strategies_den, width = 17, height = 5)
ggsave("step3_modelling/output/contexts_coeff.png", contexts_coeff, width = 10, height = 4)
ggsave("step3_modelling/output/contexts_results.png", conext_results, width = 16, height = 4)
ggsave("fig/fig2_panelA.png", contexts_raincloud, width = 11, height = 6)
