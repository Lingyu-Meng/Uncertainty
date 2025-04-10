# plot result of modelling

# List of required packages
required_packages <- c("tidyverse",
                       "sjPlot", # plot_model
                       "cowplot", # theme_cowplot
                       "ggsignif", # geom_signif
                       "lattice", # dotplot
                       "lme4", # lmer
                       "ggrain", # geom_rain
                       "emmeans", # emmeans
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

# distribution of three strategies
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

# visualise the trait-strategy relationship
## IM regardless of context
### define the values of IM for which we want to plot the marginal effects
im_values <- seq(
  from = min(trait_data$IM),
  to = max(trait_data$IM),
  length.out = 10
)

### post hoc test for UIRE: whether the effect of IM on V is significant regardless of context
IM_UIRE_p <- emtrends(
  model_IM_1,
  ~ IM,
  var = "V",
  at = list(IM = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

### estimate the marginal effects
IM_UIRE <- emmeans(
  model_IM_1,
  specs = ~ V | IM,
  at = list(V = 1, IM = im_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = IM, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Impulsive               More Impulsive →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", IM_UIRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

### post hoc test for DE: whether the effect of IM on RU is significant regardless of context
IM_DE_p <- emtrends(
  model_IM_2,
  ~ IM,
  var = "RU",
  at = list(IM = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IM_DE <- emmeans(
  model_IM_2,
  specs = ~ RU | IM,
  at = list(RU = 1, IM = im_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = IM, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Impulsive               More Impulsive →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", IM_DE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

### post hoc test for UDRE: whether the effect of IM on VTU is significant regardless of context
IM_UDRE_p <- emtrends(
  model_IM_3,
  ~ IM,
  var = "VTU",
  at = list(IM = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>%
  .$p.value %>% # Get the p value
  round(., 4)

IM_UDRE <- emmeans(
  model_IM_3,
  specs = ~ VTU | IM,
  at = list(VTU = 1, IM = im_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = IM, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Impulsive               More Impulsive →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", IM_UDRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = NULL))

## IM regarding context
### post hoc test for UIRE: whether the effect of IM on V is significant in different contexts
IM_UIRE_post_hoc <- emtrends(
  model_IM_1,
  ~ IM | context,
  var = "V",
  at = list(context = c(-0.5, 0.5), IM = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IM_UIRE_context <- emmeans(
  model_IM_1,
  specs = ~ V | IM | context,
  at = list(V = 1, IM = im_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = IM, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Impulsive               More Impulsive →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", IM_UIRE_post_hoc[1]),
      "0.5"  = paste0("p = ", IM_UIRE_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

### post hoc test for DE: whether the effect of IM on RU is significant in different contexts
IM_DE_post_hoc <- emtrends(
  model_IM_2,
  ~ IM | context,
  var = "RU",
  at = list(context = c(-0.5, 0.5), IM = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IM_DE_context <- emmeans(
  model_IM_2,
  specs = ~ RU | IM | context,
  at = list(RU = 1, IM = im_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = IM, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Impulsive               More Impulsive →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", IM_DE_post_hoc[1]),
      "0.5"  = paste0("p = ", IM_DE_post_hoc[2]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none")
  
### post hoc test for UDRE: whether the effect of IM on VTU is significant in different contexts
IM_UDRE_post_hoc <- emtrends(
  model_IM_3,
  ~ IM | context,
  var = "VTU",
  at = list(context = c(-0.5, 0.5), IM = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IM_UDRE_context <- emmeans(
  model_IM_3,
  specs = ~ VTU | IM | context,
  at = list(VTU = 1, IM = im_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = IM, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Impulsive               More Impulsive →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "0.5"  = paste0("p = ", IM_UDRE_post_hoc[2]),
      "-0.5" = paste0("p = ", IM_UDRE_post_hoc[1]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none") +
  # keep 2 digits for y axis
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

## IU regardless of context
### define the values of IU for which we want to plot the marginal effects
iu_values <- seq(
  from = min(trait_data$IU),
  to = max(trait_data$IU),
  length.out = 10
)

### post hoc test for UIRE: whether the effect of IU on V is significant regardless of context
IU_UIRE_p <- emtrends(
  model_IU_1,
  ~ IU,
  var = "V",
  at = list(IU = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

### estimate the marginal effects
IU_UIRE <- emmeans(
  model_IU_1,
  specs = ~ V | IU,
  at = list(V = 1, IU = iu_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = IU, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less IU                      More IU →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", IU_UIRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

### post hoc test for DE: whether the effect of IU on RU is significant regardless of context
IU_DE_p <- emtrends(
  model_IU_2,
  ~ IU,
  var = "RU",
  at = list(IU = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IU_DE <- emmeans(
  model_IU_2,
  specs = ~ RU | IU,
  at = list(RU = 1, IU = iu_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = IU, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less IU                      More IU →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", IU_DE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "")) +
  # keep 2 digits for y axis
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

### post hoc test for UDRE: whether the effect of IU on VTU is significant regardless of context
IU_UDRE_p <- emtrends(
  model_IU_3,
  ~ IU,
  var = "VTU",
  at = list(IU = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IU_UDRE <- emmeans(
  model_IU_3,
  specs = ~ VTU | IU,
  at = list(VTU = 1, IU = iu_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = IU, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less IU                      More IU →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", IU_UDRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

## IU regarding context
### post hoc test for UIRE: whether the effect of IU on V is significant in different contexts
IU_UIRE_post_hoc <- emtrends(
  model_IU_1,
  ~ IU | context,
  var = "V",
  at = list(context = c(-0.5, 0.5), IU = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IU_UIRE_context <- emmeans(
  model_IU_1,
  specs = ~ V | IU | context,
  at = list(V = 1, IU = iu_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = IU, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less IU                      More IU →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", IU_UIRE_post_hoc[1]),
      "0.5"  = paste0("p = ", IU_UIRE_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none")

### post hoc test for DE: whether the effect of IU on RU is significant in different contexts
IU_DE_post_hoc <- emtrends(
  model_IU_2,
  ~ IU | context,
  var = "RU",
  at = list(context = c(-0.5, 0.5), IU = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IU_DE_context <- emmeans(
  model_IU_2,
  specs = ~ RU | IU | context,
  at = list(RU = 1, IU = iu_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = IU, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less IU                      More IU →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", IU_DE_post_hoc[1]),
      "0.5"  = paste0("p = ", IU_DE_post_hoc[2]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none")

### post hoc test for UDRE: whether the effect of IU on VTU is significant in different contexts
IU_UDRE_post_hoc <- emtrends(
  model_IU_3,
  ~ IU | context,
  var = "VTU",
  at = list(context = c(-0.5, 0.5), IU = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

IU_UDRE_context <- emmeans(
  model_IU_3,
  specs = ~ VTU | IU | context,
  at = list(VTU = 1, IU = iu_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = IU, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less IU                      More IU →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "0.5"  = paste0("p = ", IU_UDRE_post_hoc[2]),
      "-0.5" = paste0("p = ", IU_UDRE_post_hoc[1]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none")

## Anx regardless of context
### define the values of Anx for which we want to plot the marginal effects
anx_values <- seq(
  from = min(trait_data$Anx),
  to = max(trait_data$Anx),
  length.out = 10
)

### post hoc test for UIRE: whether the effect of Anx on V is significant regardless of context
Anx_UIRE_p <- emtrends(
  model_Anx_1,
  ~ Anx,
  var = "V",
  at = list(Anx = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

### estimate the marginal effects
Anx_UIRE <- emmeans(
  model_Anx_1,
  specs = ~ V | Anx,
  at = list(V = 1, Anx = anx_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Anx, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Anxious               More Anxious →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", Anx_UIRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

### post hoc test for DE: whether the effect of Anx on RU is significant regardless of context
Anx_DE_p <- emtrends(
  model_Anx_2,
  ~ Anx,
  var = "RU",
  at = list(Anx = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

Anx_DE <- emmeans(
  model_Anx_2,
  specs = ~ RU | Anx,
  at = list(RU = 1, Anx = anx_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Anx, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Anxious               More Anxious →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", Anx_DE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

### post hoc test for UDRE: whether the effect of Anx on VTU is significant regardless of context
Anx_UDRE_p <- emtrends(
  model_Anx_3,
  ~ Anx,
  var = "VTU",
  at = list(Anx = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

Anx_UDRE <- emmeans(
  model_Anx_3,
  specs = ~ VTU | Anx,
  at = list(VTU = 1, Anx = anx_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Anx, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Anxious               More Anxious →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", Anx_UDRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

## Anx regarding context
### post hoc test for UIRE: whether the effect of Anx on V is significant in different contexts
Anx_UIRE_post_hoc <- emtrends(
  model_Anx_1,
  ~ Anx | context,
  var = "V",
  at = list(context = c(-0.5, 0.5), Anx = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

Anx_UIRE_context <- emmeans(
  model_Anx_1,
  specs = ~ V | Anx | context,
  at = list(V = 1, Anx = anx_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = Anx, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Anxious               More Anxious →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", Anx_UIRE_post_hoc[1]),
      "0.5"  = paste0("p = ", Anx_UIRE_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none")

### post hoc test for DE: whether the effect of Anx on RU is significant in different contexts
Anx_DE_post_hoc <- emtrends(
  model_Anx_2,
  ~ Anx | context,
  var = "RU",
  at = list(context = c(-0.5, 0.5), Anx = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

Anx_DE_context <- emmeans(
  model_Anx_2,
  specs = ~ RU | Anx | context,
  at = list(RU = 1, Anx = anx_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = Anx, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Anxious               More Anxious →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", Anx_DE_post_hoc[1]),
      "0.5"  = paste0("p = ", Anx_DE_post_hoc[2]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none") +
  # keep 2 digits for y axis
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

### post hoc test for UDRE: whether the effect of Anx on VTU is significant in different contexts
Anx_UDRE_post_hoc <- emtrends(
  model_Anx_3,
  ~ Anx | context,
  var = "VTU",
  at = list(context = c(-0.5, 0.5), Anx = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

Anx_UDRE_context <- emmeans(
  model_Anx_3,
  specs = ~ VTU | Anx | context,
  at = list(VTU = 1, Anx = anx_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = Anx, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Anxious               More Anxious →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "0.5"  = paste0("p = ", Anx_UDRE_post_hoc[2]),
      "-0.5" = paste0("p = ", Anx_UDRE_post_hoc[1]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none") +
  # keep 2 digits for y axis
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

## RA regardless of context
### define the values of RA for which we want to plot the marginal effects
ra_values <- seq(
  from = min(trait_data$RA),
  to = max(trait_data$RA),
  length.out = 10
)

### post hoc test for UIRE: whether the effect of RA on V is significant regardless of context
RA_UIRE_p <- emtrends(
  model_RA_1,
  ~ RA,
  var = "V",
  at = list(RA = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

### estimate the marginal effects
RA_UIRE <- emmeans(
  model_RA_1,
  specs = ~ V | RA,
  at = list(V = 1, RA = ra_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = RA, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Risk Aversive           More Risk Aversive →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", RA_UIRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

### post hoc test for DE: whether the effect of RA on RU is significant regardless of context
RA_DE_p <- emtrends(
  model_RA_2,
  ~ RA,
  var = "RU",
  at = list(RA = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

RA_DE <- emmeans(
  model_RA_2,
  specs = ~ RU | RA,
  at = list(RU = 1, RA = ra_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = RA, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Risk Aversive           More Risk Aversive →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", RA_DE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

### post hoc test for UDRE: whether the effect of RA on VTU is significant regardless of context
RA_UDRE_p <- emtrends(
  model_RA_3,
  ~ RA,
  var = "VTU",
  at = list(RA = c(0, 1)),
  type = "response"
) %>% 
  pairs() %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

RA_UDRE <- emmeans(
  model_RA_3,
  specs = ~ VTU | RA,
  at = list(VTU = 1, RA = ra_values, context = 0),
  type = "response"
) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = RA, y = prob)) +
  geom_line(aes(colour = "black")) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Risk Aversive           More Risk Aversive →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  scale_colour_manual(values = "black",
                      labels = paste0("p = ", RA_UDRE_p)
  ) +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = ""))

## RA regarding context
### post hoc test for UIRE: whether the effect of RA on V is significant in different contexts
RA_UIRE_post_hoc <- emtrends(
  model_RA_1,
  ~ RA | context,
  var = "V",
  at = list(context = c(-0.5, 0.5), RA = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

RA_UIRE_context <- emmeans(
  model_RA_1,
  specs = ~ V | RA | context,
  at = list(V = 1, RA = ra_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = RA, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Risk Aversive           More Risk Aversive →",
       y = "← Less UIRE                    More UIRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", RA_UIRE_post_hoc[1]),
      "0.5"  = paste0("p = ", RA_UIRE_post_hoc[2]))
  ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none") +
  # keep 2 digits for y axis
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

### post hoc test for DE: whether the effect of RA on RU is significant in different contexts
RA_DE_post_hoc <- emtrends(
  model_RA_2,
  ~ RA | context,
  var = "RU",
  at = list(context = c(-0.5, 0.5), RA = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4) %>% 
  format(scientific = FALSE)

RA_DE_context <- emmeans(
  model_RA_2,
  specs = ~ RU | RA | context,
  at = list(RU = 1, RA = ra_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = RA, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Risk Aversive           More Risk Aversive →",
       y = "← Less DE                      More DE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "-0.5" = paste0("p = ", RA_DE_post_hoc[1]),
      "0.5"  = paste0("p = ", RA_DE_post_hoc[2]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none")

### post hoc test for UDRE: whether the effect of RA on VTU is significant in different contexts
RA_UDRE_post_hoc <- emtrends(
  model_RA_3,
  ~ RA | context,
  var = "VTU",
  at = list(context = c(-0.5, 0.5), RA = c(0, 1)),
  type = "response"
) %>% 
  pairs(by = "context") %>% 
  summary() %>% .$p.value %>% # Get the p value
  round(., 4)

RA_UDRE_context <- emmeans(
  model_RA_3,
  specs = ~ VTU | RA | context,
  at = list(VTU = 1, RA = ra_values, context = c(0.5, -0.5)),
  type = "response"
) %>% 
  as.data.frame() %>% 
  mutate(context = factor(context, levels = c(0.5, -0.5))) %>% # 0.5:loss, -0.5:win
  ggplot(aes(x = RA, y = prob)) +
  geom_line(aes(colour = context)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = context), alpha = 0.2) +
  theme_cowplot() +
  labs(x = "← Less Risk Aversive           More Risk Aversive →",
       y = "← Less UDRE                    More UDRE→") +
  xlim(-3, 3) +
  theme(legend.position = "top") +
  scale_colour_discrete(
    labels = c(
      "0.5"  = paste0("p = ", RA_UDRE_post_hoc[2]),
      "-0.5" = paste0("p = ", RA_UDRE_post_hoc[1]))
    ) +
  guides(colour = guide_legend(title = NULL),
         fill = "none") +
  # keep 2 digits for y axis
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

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
  transmute(`UIRE in Loss` = V + 0.5 * `V:context`,
            `UIRE in Win`  = V - 0.5 * `V:context`,
            `DE in Loss` = RU + 0.5 * `RU:context`,
            `DE in Win`  = RU - 0.5 * `RU:context`,
            `UDRE in Loss` = VTU + 0.5 * `VTU:context`,
            `UDRE in Win`  = VTU - 0.5 * `VTU:context`
  ) %>%
  gather(key = "Strategy", value = "Coefficient") %>%
  mutate(Context = ifelse(str_detect(Strategy, "Loss"), "Loss", "Win"),
         Strategy = str_remove(Strategy, " in Loss| in Win")) %>%
  ggplot(aes(x = Strategy, y = Coefficient, fill = Context, colour = Context)) +
  # drow raincloud and aviod overlap boxplot
  geom_rain(alpha = 0.5, point.args = list(alpha = .2),
            boxplot.args.pos = list(width = 0.04, position = position_nudge(x = c(0.07, 0.12)))  # Manual dodge
            ) +
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
ggsave("fig/fig2_panelA.png", contexts_raincloud, width = 8, height = 6)
ggsave("fig/fig3_panelB.png", IM_DE, width = 5, height = 5)
ggsave("fig/fig3_panelC.png", IM_UDRE, width = 5, height = 5)
ggsave("fig/fig3_panelD.png", IM_UIRE, width = 5, height = 5)
ggsave("fig/fig3_panelG.png", IM_DE_context, width = 5, height = 5)
ggsave("fig/fig3_panelH.png", IM_UDRE_context, width = 5, height = 5)
ggsave("fig/fig3_panelI.png", IM_UIRE_context, width = 5, height = 5)
ggsave("fig/fig4_panelB.png", IU_DE, width = 5, height = 5)
ggsave("fig/fig4_panelC.png", IU_UDRE, width = 5, height = 5)
ggsave("fig/fig4_panelD.png", IU_UIRE, width = 5, height = 5)
ggsave("fig/fig4_panelG.png", IU_DE_context, width = 5, height = 5)
ggsave("fig/fig4_panelH.png", IU_UDRE_context, width = 5, height = 5)
ggsave("fig/fig4_panelI.png", IU_UIRE_context, width = 5, height = 5)
ggsave("fig/fig5_panelB.png", Anx_DE, width = 5, height = 5)
ggsave("fig/fig5_panelC.png", Anx_UDRE, width = 5, height = 5)
ggsave("fig/fig5_panelD.png", Anx_UIRE, width = 5, height = 5)
ggsave("fig/fig5_panelG.png", Anx_DE_context, width = 5, height = 5)
ggsave("fig/fig5_panelH.png", Anx_UDRE_context, width = 5, height = 5)
ggsave("fig/fig5_panelI.png", Anx_UIRE_context, width = 5, height = 5)
ggsave("fig/fig6_panelB.png", RA_DE, width = 5, height = 5)
ggsave("fig/fig6_panelC.png", RA_UDRE, width = 5, height = 5)
ggsave("fig/fig6_panelD.png", RA_UIRE, width = 5, height = 5)
ggsave("fig/fig6_panelG.png", RA_DE_context, width = 5, height = 5)
ggsave("fig/fig6_panelH.png", RA_UDRE_context, width = 5, height = 5)
ggsave("fig/fig6_panelI.png", RA_UIRE_context, width = 5, height = 5)
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