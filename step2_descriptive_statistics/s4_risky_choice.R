# Model free analysis on the risky arm task

required_packages <- c("tidyverse", "cowplot", "sjPlot")

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

## Load data
risky_arm <- read_csv("step2_descriptive_statistics/output/kalman_data.csv") %>% 
  mutate(
    selection = if_else(response == "Left", left, right),
    selection = case_when(
      grepl("R", selection) ~ "R",
      grepl("r", selection) ~ "r",
      grepl("S", selection) ~ "S",
    ),
    risky = if_else(selection == "R", 1, 0),
    context = factor(context, levels = c("Win", "Lose"))
  )

## All traits
risky_glmm_data <- risky_arm %>% 
  mutate(
    context = case_when(
      grepl("Win", context) ~ -0.5,
      grepl("Lose", context) ~ 0.5,
    ),
    arms = case_when(
      grepl("r", arms) ~ -0.5,
      grepl("S", arms) ~ 0.5,
    )
  )
risky_glmm <-  glmer(risky ~ (IU + IM + Anx + RA) * (context + arms) +
          (1 | ID),
      data = risky_glmm_data, family = binomial(link = "logit"))
HLM_summary(risky_glmm)
risky_glmm_fig <- plot_model(risky_glmm,
                            title = "Choosing the Risky Arm",
                            show.values = TRUE,
                            value.offset = 0.4,
                            sort.est = TRUE) +
  theme_bw() +
  ylim(0.5, 1.5)

print_table(risky_glmm, file = "step2_descriptive_statistics/output/risky_glmm_summary.doc")
# scatter plot
# If facet_wrap is bad, use the commented code below
# IU_risky <- risky_arm %>% 
#   group_by(ID) %>% 
#   summarise(
#     IU = mean(IU),
#     Risky_rate = mean(risky)
#   ) %>% 
#   ggplot(aes(x = IU, y = Risky_rate)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_cowplot() +
#   labs(
#     x = "Intolerance of Uncertainty",
#     y = "Risky Arm Selection Rate"
#   )
# 
# IM_risky <- risky_arm %>%
#   group_by(ID) %>%
#   summarise(
#     IM = mean(IM),
#     Risky_rate = mean(risky)
#   ) %>%
#   ggplot(aes(x = IM, y = Risky_rate)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_cowplot() +
#   labs(
#     x = "Impulsivity",
#     y = "Risky Arm Selection Rate"
#   )
# 
# Anx_risky <- risky_arm %>%
#   group_by(ID) %>%
#   summarise(
#     Anx = mean(Anx),
#     Risky_rate = mean(risky)
#   ) %>%
#   ggplot(aes(x = Anx, y = Risky_rate)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_cowplot() +
#   labs(
#     x = "Anxiety",
#     y = "Risky Arm Selection Rate"
#   )
# 
# RA_risky <- risky_arm %>%
#   group_by(ID) %>%
#   summarise(
#     RA = mean(RA),
#     Risky_rate = mean(risky)
#   ) %>%
#   ggplot(aes(x = RA, y = Risky_rate)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_cowplot() +
#   labs(
#     x = "Risk Aversion",
#     y = "Risky Arm Selection Rate"
#   )

traits_risky <- risky_arm %>%
  group_by(ID) %>%
  summarise(
    IU = mean(IU),
    IM = mean(IM),
    Anx = mean(Anx),
    RA = mean(RA),
    Risky_rate = mean(risky)
  ) %>%
  gather(key = "trait", value = "value", IU:RA) %>%
  ggplot(aes(x = value, y = Risky_rate, color = trait)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~trait, scales = "free_x", nrow = 1) +
  labs(
    x = "Trait",
    y = "Risky Arm Selection Rate"
  )

traits_risky_context <- risky_arm %>%
  group_by(ID, context) %>%
  summarise(
    IU = mean(IU),
    IM = mean(IM),
    Anx = mean(Anx),
    RA = mean(RA),
    Risky_rate = mean(risky)
  ) %>%
  gather(key = "trait", value = "value", IU:RA) %>%
  ggplot(aes(x = value, y = Risky_rate, color = trait, shape = context)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_grid(context~trait, scales = "free_x") +
  labs(
    x = "Trait",
    y = "Risky Arm Selection Rate"
  )

risky_arm_fig <- cowplot::plot_grid( # sjPlot has plot_grid as well
  risky_glmm_fig,
  traits_risky_context,
  rel_widths = c(1, 2),
  labels = c('A', 'B'))

# save plots
ggsave("step2_descriptive_statistics/output/traits_risky_context.png", traits_risky_context, width = 10, height = 5)
ggsave("step2_descriptive_statistics/output/risky_glmm_fig.png", risky_glmm_fig, width = 7, height = 7)
ggsave("step2_descriptive_statistics/output/risky_arm_fig.png", risky_arm_fig, width = 18, height = 6)