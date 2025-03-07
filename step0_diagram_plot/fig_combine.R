# all analysis code should be run first
# check RDS files in the fig folder

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

# fig2
## read all rds files starting with "fig2" in the fig folder
fig2_files <- list.files(save_path, pattern = "fig2.*.rds", full.names = TRUE)
lapply(fig2_files, readRDS)

## combine all the panels
fig2_bottom <- cowplot::plot_grid(
  intercepts, slopes, rain_meanlnRT_context_arms,
  labels = c("B", "C", "D"),
  ncol = 3, align = "hv"
)

fig2_top <- cowplot::plot_grid(
  NULL, contexts_raincloud, NULL,
  labels = c("", "A", ""),
  ncol = 3,
  rel_widths = c(1, 8, 1)
)

fig2 <- cowplot::plot_grid(
  fig2_top, fig2_bottom,
  labels = c("", ""),
  ncol = 1, align = "hv"
)

## save the combined figure
ggsave(file.path(save_path, "fig2.png"), fig2, width = 13, height = 7)

# fig3
## read all rds files starting with "fig3" in the fig folder
fig3_files <- list.files(save_path, pattern = "fig3.*.rds", full.names = TRUE)
lapply(fig3_files, readRDS)

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  correct_trait_glmm_IM_inter +
    theme(legend.box.margin = margin(0, 0, 0, 12)) +
    guides(colour = guide_legend(nrow = 1))
)

fig3_f <- cowplot::plot_grid(
  correct_trait_glmm_IM_effect, IM_DE, IM_UDE, IM_UIE, RT_IM,
  correct_trait_glmm_IM_inter + theme(legend.position = "none"),
  IM_DE_context + theme(legend.position = "none"),
  IM_UDE_context + theme(legend.position = "none"),
  IM_UIE_context + theme(legend.position = "none"),
  IM_RT_context + theme(legend.position = "none"),
  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  ncol = 5, align = "hv"
)
fig3 <- cowplot::plot_grid(
  fig3_f, legend,
  labels = c("", ""),
  ncol = 1, align = "hv",
  rel_heights = c(1, 0.05)
)

## save the combined figure
ggsave(file.path(save_path, "fig3.png"), fig3, width = 21, height = 10)

# fig4
## read all rds files starting with "fig4" in the fig folder
fig4_files <- list.files(save_path, pattern = "fig4.*.rds", full.names = TRUE)
lapply(fig4_files, readRDS)

fig4_f <- cowplot::plot_grid(
  correct_trait_glmm_IU_effect, IU_DE, IU_UDE, IU_UIE, RT_IU,
  correct_trait_glmm_IU_inter + theme(legend.position = "none"),
  IU_DE_context + theme(legend.position = "none"),
  IU_UDE_context + theme(legend.position = "none"),
  IU_UIE_context + theme(legend.position = "none"),
  IU_RT_context + theme(legend.position = "none"),
  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  ncol = 5, align = "hv"
)
fig4 <- cowplot::plot_grid(
  fig4_f, legend,
  labels = c("", ""),
  ncol = 1, align = "hv",
  rel_heights = c(1, 0.05)
)

## save the combined figure
ggsave(file.path(save_path, "fig4.png"), fig4, width = 21, height = 10)

# fig5
## read all rds files starting with "fig5" in the fig folder
fig5_files <- list.files(save_path, pattern = "fig5.*.rds", full.names = TRUE)
lapply(fig5_files, readRDS)

fig5_f <- cowplot::plot_grid(
  correct_trait_glmm_Anx_effect, Anx_DE, Anx_UDE, Anx_UIE, RT_anx,
  correct_trait_glmm_Anx_inter + theme(legend.position = "none"),
  Anx_DE_context + theme(legend.position = "none"),
  Anx_UDE_context + theme(legend.position = "none"),
  Anx_UIE_context + theme(legend.position = "none"),
  Anxi_RT_context + theme(legend.position = "none"),
  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  ncol = 5, align = "hv"
)
fig5 <- cowplot::plot_grid(
  fig5_f, legend,
  labels = c("", ""),
  ncol = 1, align = "hv",
  rel_heights = c(1, 0.05)
)

## save the combined figure
ggsave(file.path(save_path, "fig5.png"), fig5, width = 21, height = 10)

# fig6
## read all rds files starting with "fig6" in the fig folder
fig6_files <- list.files(save_path, pattern = "fig6.*.rds", full.names = TRUE)
lapply(fig6_files, readRDS)

fig6_f <- cowplot::plot_grid(
  correct_trait_glmm_RA_effect, RA_DE, RA_UDE, RA_UIE, RT_risk,
  correct_trait_glmm_RA_inter + theme(legend.position = "none"),
  RA_DE_context + theme(legend.position = "none"),
  RA_UDE_context + theme(legend.position = "none"),
  RA_UIE_context + theme(legend.position = "none"),
  RA_RT_context + theme(legend.position = "none"),
  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  ncol = 5, align = "hv"
)
fig6 <- cowplot::plot_grid(
  fig6_f, legend,
  labels = c("", ""),
  ncol = 1, align = "hv",
  rel_heights = c(1, 0.05)
)

## save the combined figure
ggsave(file.path(save_path, "fig6.png"), fig6, width = 21, height = 10)