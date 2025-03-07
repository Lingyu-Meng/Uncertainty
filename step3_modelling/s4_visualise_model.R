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

trait_data <- read_csv("step3_modelling/output/data.csv") %>% 
  select(ID, IU, IM, Anx, RA) %>%
  unique() %>% 
  mutate(ID = as.character(ID))

trait_strategy <- coef(context_model)$ID %>% # Get the real slope for everyone
  as.data.frame() %>%
  rownames_to_column(var = "ID") %>%
  left_join(trait_data, by = "ID")

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

# restore point from the model
## IM
IM_strategy <- coef(model_IM_4)$ID %>% # Get the real slope for everyone
  as.data.frame() %>%
  rownames_to_column(var = "ID") %>%
  left_join(trait_data, by = "ID") %>% 
  mutate(DE = RU + IM * `RU:IM`,
         UDE = VTU + IM * `VTU:IM`,
         UIE = V + IM * `V:IM`)
IM_strategy_context <- IM_strategy %>% 
  mutate(DE_loss = DE + 0.5 * `RU:context` + 0.5 * IM * `RU:IM:context`,
         DE_win = DE - 0.5 * `RU:context` - 0.5 * IM * `RU:IM:context`,
         UDE_loss = UDE + 0.5 * `VTU:context` + 0.5 * IM * `VTU:IM:context`,
         UDE_win = UDE - 0.5 * `VTU:context` - 0.5 * IM * `VTU:IM:context`,
         UIE_loss = UIE + 0.5 * `V:context` + 0.5 * IM * `V:IM:context`,
         UIE_win = UIE - 0.5 * `V:context` - 0.5 * IM * `V:IM:context`) %>%
  # transfer V_win: VTU_loss to Strategy: V RU VTU; context: Loss Win
  gather(key = "Strategy", value = "value", DE_loss:UIE_win) %>%
  separate(Strategy, c("Strategy", "Context"), sep = "_")

IM_DE <- IM_strategy %>%
  ggplot(aes(x = IM, y = DE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IM",
       y = "Directed exploration") +
  xlim(-3, 3)

IM_DE_context <- IM_strategy_context %>% 
  filter(Strategy == "DE") %>%
  ggplot(aes(x = IM, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IM",
       y = "Directed exploration") +
  xlim(-3, 3)

IM_UDE <- IM_strategy %>%
  ggplot(aes(x = IM, y = UDE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IM",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

IM_UDE_context <- IM_strategy_context %>%
  filter(Strategy == "UDE") %>%
  ggplot(aes(x = IM, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IM",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

IM_UIE <- IM_strategy %>%
  ggplot(aes(x = IM, y = UIE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IM",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

IM_UIE_context <- IM_strategy_context %>%
  filter(Strategy == "UIE") %>%
  ggplot(aes(x = IM, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IM",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

## IU
IU_strategy <- coef(model_IU_4)$ID %>% # Get the real slope for everyone
  as.data.frame() %>%
  rownames_to_column(var = "ID") %>%
  left_join(trait_data, by = "ID") %>% 
  mutate(DE = RU + IU * `RU:IU`,
         UDE = VTU + IU * `VTU:IU`,
         UIE = V + IU * `V:IU`)
IU_strategy_context <- IU_strategy %>%
  mutate(DE_loss = DE + 0.5 * `RU:context` + 0.5 * IU * `RU:IU:context`,
         DE_win = DE - 0.5 * `RU:context` - 0.5 * IU * `RU:IU:context`,
         UDE_loss = UDE + 0.5 * `VTU:context` + 0.5 * IU * `VTU:IU:context`,
         UDE_win = UDE - 0.5 * `VTU:context` - 0.5 * IU * `VTU:IU:context`,
         UIE_loss = UIE + 0.5 * `V:context` + 0.5 * IU * `V:IU:context`,
         UIE_win = UIE - 0.5 * `V:context` - 0.5 * IU * `V:IU:context`) %>%
  gather(key = "Strategy", value = "value", DE_loss:UIE_win) %>%
  separate(Strategy, c("Strategy", "Context"), sep = "_")

IU_DE <- IU_strategy %>%
  ggplot(aes(x = IU, y = DE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IU",
       y = "Directed exploration") +
  xlim(-3, 3)

IU_DE_context <- IU_strategy_context %>%
  filter(Strategy == "DE") %>%
  ggplot(aes(x = IU, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IU",
       y = "Directed exploration") +
  xlim(-3, 3)

IU_UDE <- IU_strategy %>%
  ggplot(aes(x = IU, y = UDE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IU",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

IU_UDE_context <- IU_strategy_context %>%
  filter(Strategy == "UDE") %>%
  ggplot(aes(x = IU, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IU",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

IU_UIE <- IU_strategy %>%
  ggplot(aes(x = IU, y = UIE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IU",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

IU_UIE_context <- IU_strategy_context %>%
  filter(Strategy == "UIE") %>%
  ggplot(aes(x = IU, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "IU",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

## Anx
Anx_strategy <- coef(model_Anx_4)$ID %>% # Get the real slope for everyone
  as.data.frame() %>%
  rownames_to_column(var = "ID") %>%
  left_join(trait_data, by = "ID") %>% 
  mutate(DE = RU + Anx * `RU:Anx`,
         UDE = VTU + Anx * `VTU:Anx`,
         UIE = V + Anx * `V:Anx`)
Anx_strategy_context <- Anx_strategy %>%
  mutate(DE_loss = DE + 0.5 * `RU:context` + 0.5 * Anx * `RU:Anx:context`,
         DE_win = DE - 0.5 * `RU:context` - 0.5 * Anx * `RU:Anx:context`,
         UDE_loss = UDE + 0.5 * `VTU:context` + 0.5 * Anx * `VTU:Anx:context`,
         UDE_win = UDE - 0.5 * `VTU:context` - 0.5 * Anx * `VTU:Anx:context`,
         UIE_loss = UIE + 0.5 * `V:context` + 0.5 * Anx * `V:Anx:context`,
         UIE_win = UIE - 0.5 * `V:context` - 0.5 * Anx * `V:Anx:context`) %>%
  gather(key = "Strategy", value = "value", DE_loss:UIE_win) %>%
  separate(Strategy, c("Strategy", "Context"), sep = "_")

Anx_DE <- Anx_strategy %>%
  ggplot(aes(x = Anx, y = DE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "Anx",
       y = "Directed exploration") +
  xlim(-3, 3)

Anx_DE_context <- Anx_strategy_context %>%
  filter(Strategy == "DE") %>%
  ggplot(aes(x = Anx, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "Anx",
       y = "Directed exploration") +
  xlim(-3, 3)

Anx_UDE <- Anx_strategy %>%
  ggplot(aes(x = Anx, y = UDE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "Anx",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

Anx_UDE_context <- Anx_strategy_context %>%
  filter(Strategy == "UDE") %>%
  ggplot(aes(x = Anx, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "Anx",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

Anx_UIE <- Anx_strategy %>%
  ggplot(aes(x = Anx, y = UIE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "Anx",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

Anx_UIE_context <- Anx_strategy_context %>%
  filter(Strategy == "UIE") %>%
  ggplot(aes(x = Anx, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "Anx",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

## RA
RA_strategy <- coef(model_RA_2)$ID %>% # Get the real slope for everyone
  as.data.frame() %>%
  rownames_to_column(var = "ID") %>%
  left_join(trait_data, by = "ID") %>% 
  mutate(DE = RU + RA * `RU:RA`,
         UDE = VTU + RA * `VTU:RA`,
         UIE = V + RA * `V:RA`)
RA_strategy_context <- RA_strategy %>%
  mutate(DE_loss = DE + 0.5 * `RU:context` + 0.5 * RA * `RU:RA:context`,
         DE_win = DE - 0.5 * `RU:context` - 0.5 * RA * `RU:RA:context`,
         UDE_loss = UDE + 0.5 * `VTU:context` + 0.5 * RA * `VTU:RA:context`,
         UDE_win = UDE - 0.5 * `VTU:context` - 0.5 * RA * `VTU:RA:context`,
         UIE_loss = UIE + 0.5 * `V:context` + 0.5 * RA * `V:RA:context`,
         UIE_win = UIE - 0.5 * `V:context` - 0.5 * RA * `V:RA:context`) %>%
  gather(key = "Strategy", value = "value", DE_loss:UIE_win) %>%
  separate(Strategy, c("Strategy", "Context"), sep = "_")

RA_DE <- RA_strategy %>%
  ggplot(aes(x = RA, y = DE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "RA",
       y = "Directed exploration") +
  xlim(-3, 3)

RA_DE_context <- RA_strategy_context %>%
  filter(Strategy == "DE") %>%
  ggplot(aes(x = RA, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "RA",
       y = "Directed exploration") +
  xlim(-3, 3)

RA_UDE <- RA_strategy %>%
  ggplot(aes(x = RA, y = UDE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "RA",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

RA_UDE_context <- RA_strategy_context %>%
  filter(Strategy == "UDE") %>%
  ggplot(aes(x = RA, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "RA",
       y = "Uncertainty-dependent random exploration") +
  xlim(-3, 3)

RA_UIE <- RA_strategy %>%
  ggplot(aes(x = RA, y = UIE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "RA",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

RA_UIE_context <- RA_strategy_context %>%
  filter(Strategy == "UIE") %>%
  ggplot(aes(x = RA, y = value, colour = Context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot() +
  labs(x = "RA",
       y = "Uncertainty-independent random exploration") +
  xlim(-3, 3)

# visualise trait free model
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

## context model with data points (random effects)
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

# Save
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
saveRDS(contexts_raincloud, "fig/fig2_panelA.rds")
saveRDS(IM_DE, "fig/fig3_panelB.rds")
saveRDS(IM_UDE, "fig/fig3_panelC.rds")
saveRDS(IM_UIE, "fig/fig3_panelD.rds")
saveRDS(IU_DE, "fig/fig4_panelB.rds")
saveRDS(IU_UDE, "fig/fig4_panelC.rds")
saveRDS(IU_UIE, "fig/fig4_panelD.rds")
saveRDS(Anx_DE, "fig/fig5_panelB.rds")
saveRDS(Anx_UDE, "fig/fig5_panelC.rds")
saveRDS(Anx_UIE, "fig/fig5_panelD.rds")
saveRDS(RA_DE, "fig/fig6_panelB.rds")
saveRDS(RA_UDE, "fig/fig6_panelC.rds")
saveRDS(RA_UIE, "fig/fig6_panelD.rds")
saveRDS(IM_DE_context, "fig/fig3_panelG.rds")
saveRDS(IM_UDE_context, "fig/fig3_panelH.rds")
saveRDS(IM_UIE_context, "fig/fig3_panelI.rds")
saveRDS(IU_DE_context, "fig/fig4_panelG.rds")
saveRDS(IU_UDE_context, "fig/fig4_panelH.rds")
saveRDS(IU_UIE_context, "fig/fig4_panelI.rds")
saveRDS(Anx_DE_context, "fig/fig5_panelG.rds")
saveRDS(Anx_UDE_context, "fig/fig5_panelH.rds")
saveRDS(Anx_UIE_context, "fig/fig5_panelI.rds")
saveRDS(RA_DE_context, "fig/fig6_panelG.rds")
saveRDS(RA_UDE_context, "fig/fig6_panelH.rds")
saveRDS(RA_UIE_context, "fig/fig6_panelI.rds")