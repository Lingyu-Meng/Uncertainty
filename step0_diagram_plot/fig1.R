# draw figures for illustration

required_packages <- c("tidyverse","cowplot")

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

save_path <- "fig"

# Panel B, distribution of the reward under the two conditions
## frequency of the reward in S arm r arm and R arm
R <- c(1, 2, 3, 3, 4, 4, 4, 4, 5, 5, 6, 7)
r <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3)
S <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)

## plot the probability distribution of the reward
df_rR <- data.frame(R = R, r = r) %>%
  gather(key = "condition", value = "reward")
df_SR <- data.frame(R = R, S = S) %>%
  gather(key = "condition", value = "reward")

panel_B <- df_rR %>%
  ggplot(aes(x = reward, fill = condition)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
### add kerbal density estimation for R and r
  geom_density(data = df %>% filter(condition %in% c("R", "r")),
               aes(y = ..count..), alpha = 0.3,
               adjust = 3) +
  labs(x = "Reward", y = "Frequency", fill = "Arm") +
  scale_fill_manual(values = c("green", "red")) +
  theme_cowplot() +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  scale_y_continuous(breaks = seq(0, 12, 3)) +
  theme(legend.position = "top") +
  ylim(0, 11.5) # it is weird that the ylim should be 11.5 to make it as 12

panel_C <- df_SR %>%
  ggplot(aes(x = reward, fill = condition)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.5) +
### add kerbal density estimation for R
  geom_density(data = df %>% filter(condition %in% "R"),
               aes(y = ..count..), alpha = 0.3,
               adjust = 3) +
  labs(x = "Reward", y = "Frequency", fill = "Arm") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_cowplot() +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  scale_y_continuous(breaks = seq(0, 12, 3)) +
  theme(legend.position = "top")

ggsave(file.path(save_path, "fig1_panel_B.png"), panel_B, width = 5, height = 5)
ggsave(file.path(save_path, "fig1_panel_C.png"), panel_C, width = 5, height = 5)

# Panel D, psychometric curve for directed exploration, uncertainty-dependent random exploration, and uncertainty-independent random exploration
## Directed exploration
V <- seq(-6, 6, 0.01)
no_dir   <- pnorm(V + 0 + V/ 1.72) # 1.72 is the standard deviation of the population (TU)
more_dir <- pnorm(V + 1 + V/ 1.72)
negate_dir <- pnorm(V - 1 + V/ 1.72)
data_dir <- data.frame(V, more_dir, no_dir, negate_dir) %>% 
  gather(key = "condition", value = "P", -V) %>% 
  mutate(condition = factor(condition, levels = c("more_dir", "no_dir", "negate_dir")))

SR <- pnorm(V + 0.5 + V/ 1.72)
rR <- pnorm(V + 0.5 * 0.55 + V/ 1.89) # 1.89 is the standard deviation of the population (TU)
# RU(rR)/RU(SR) ≈ 0.55
data_SR_rR <- data.frame(V, SR, rR) %>% 
  gather(key = "condition", value = "P", -V)

directed_ex <- ggplot(data_dir, aes(x = V, y = P, linetype = condition)) +
  geom_line(linewidth = 2, alpha = 0.5) +
  labs(x = "Expected value difference V(1) - V(2)",
       y = "Choice Probability P(1)", linetype = "DE") +
  scale_linetype_manual(values = c("solid", "dotdash", "dotted"),
                        labels = c("Risk Seeking", "Risk neutral", "Risk Averse")) +
  theme_cowplot() +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme(legend.position = "top") +
  theme(legend.key.width = unit(1.5,"cm"))

ggsave(file.path(save_path, "fig1_panel_C_directed_exploration.png"), directed_ex, width = 7, height = 7)

## Uncertainty-dependent random exploration
no_UDR   <- pnorm(1 * V + 0 + 0 * V/ 1.72) # 1.72 is the standard deviation of the population (TU)
more_UDR <- pnorm(0 * V + 0 + 1 * V/ 1.72)
no_UDR_high_uncertainty   <- pnorm(1 * V + 0 + 0 * V/ 5)
more_UDR_high_uncertainty <- pnorm(0 * V + 0 + 1 * V/ 5)
# gater in 2 key: level of UDR and level of uncertainty
data_UDR <- data.frame(V, no_UDR, more_UDR, no_UDR_high_uncertainty, more_UDR_high_uncertainty) %>% 
  gather(key = "condition", value = "P", -V) %>% 
  mutate(uncertainty = ifelse(condition %in% c("no_UDR", "more_UDR"), "Low", "High"),
         UDR = ifelse(condition %in% c("no_UDR", "no_UDR_high_uncertainty"), "No", "High"))

UDR_ex <- ggplot(data_UDR, aes(x = V, y = P, linetype = UDR, colour = uncertainty)) +
  geom_line(linewidth = 2, alpha = 0.5) +
  labs(x = "Expected value difference V(1) - V(2)",
       y = "Choice Probability P(1)",
       linetype = "UDRE",
       colour = "Uncertainty") +
  scale_linetype_manual(values = c("solid", "dotdash")) +
  theme_cowplot() +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme(legend.position = "top")

ggsave(file.path(save_path, "fig1_panel_C_UDR_exploration.png"), UDR_ex, width = 7, height = 7)

## Uncertainty-independent random exploration
no_UIR   <- pnorm(0 * V + 0 + 1 * V/ 1.72) # 1.72 is the standard deviation of the population (TU)
more_UIR <- pnorm(1 * V + 0 + 0 * V/ 1.72)
no_UIR_high_uncertainty   <- pnorm(0 * V + 0 + 1 * V/ 5)
more_UIR_high_uncertainty <- pnorm(1 * V + 0 + 0 * V/ 5)
# gater in 2 key: level of UIR and level of uncertainty
data_UIR <- data.frame(V, no_UIR, more_UIR, no_UIR_high_uncertainty, more_UIR_high_uncertainty) %>% 
  gather(key = "condition", value = "P", -V) %>% 
  mutate(uncertainty = ifelse(condition %in% c("no_UIR", "more_UIR"), "Low", "High"),
         UIR = ifelse(condition %in% c("no_UIR", "no_UIR_high_uncertainty"), "No", "High"))

UIR_ex <- ggplot(data_UIR, aes(x = V, y = P, linetype = UIR, colour = uncertainty)) +
  geom_line(linewidth = 2, alpha = 0.5) +
  labs(x = "Expected value difference V(1) - V(2)",
       y = "Choice Probability P(1)",
       linetype = "UIRE",
       colour = "Uncertainty") +
  scale_linetype_manual(values = c("solid", "dotdash")) +
  theme_cowplot() +
  scale_x_continuous(breaks = seq(-6, 6, 2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme(legend.position = "top")

ggsave(file.path(save_path, "fig1_panel_C_UIR_exploration.png"), UIR_ex, width = 7, height = 7)

# combine the four figures
panel_BC <- plot_grid(panel_B, directed_ex, UDR_ex, UIR_ex, ncol = 2)
ggsave(file.path(save_path, "fig1_panel_BC.png"), panel_BC, width = 14, height = 14)