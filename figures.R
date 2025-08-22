library(tidyverse)
library(tmle)
library(patchwork)

##### Example Figure #####
source("simulation_study_2/simulate.R")
source("R/estimator_group_effects.R")

colors = c(
  "L2 penalty" = "#3498db", 
  "L1 penalty" = "#27ae60", 
  "No penalty" = "#2c3e50",
  "Empirical Bayes" = "red",
  "True effect" = "black"
)

plot_sample_size <- function(target_sample_size = "small") {
  levels <- c("No penalty", "L1 penalty", "L2 penalty", "Empirical Bayes", "True effect")
  fits %>%
    filter(method != "l0", sample_size == target_sample_size) %>%
    mutate(method = case_when(
      method == "l1" ~ "L1 penalty",
      method == "l2" ~ "L2 penalty",
      method == "empirical_bayes" ~ "Empirical Bayes",
      method == "unpenalized" ~ "No penalty",
    )) %>%
    mutate(method = factor(method, levels = levels)) %>%
    ggplot(aes(y = psi, x = reorder(group, true_psi), color = method)) +
    geom_hline(yintercept = 0, alpha = 0.1) +
    geom_point(aes(y = true_psi, color = factor(c("True effect"), levels = levels)), shape = 4, size = 2.5) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5), alpha = 0.5) +
    scale_color_manual(values = colors) +
    theme(
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      legend.title = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size = 12)
    ) +
    labs(x = "Group", y = "Group-specific\nAverage Treatment Effect") +
    ylim(range(c(fits$psi, fits$lower, fits$upper)))
    #ylim(range(c(fits$psi))) 
}

plot_method <- function(target_method = "l2", target_sample_size = "small") {
  if(target_method == "unpenalized") {
    fits %>%
      filter(method == "unpenalized", sample_size == target_sample_size) %>%
      ggplot(aes(x = true_psi, y = reorder(group, true_psi))) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = "Unpenalized\nEstimate"), position = position_nudge(y = -nudge), height = 0, alpha = 0.5) +
      geom_point(aes(x = psi, color = "Unpenalized\nEstimate"), position = position_nudge(y = -nudge)) +
      geom_point(aes(color = "Truth"), shape = 18, size = 2.5) +
      labs(y = "Group", x = "Group-Specific\nAverage Treatment Effect") +
      scale_color_manual(values = colors, breaks = names(colors)) +
      my_theme +
      xlim(range(c(fits$psi, fits$lower, fits$upper)))
      #xlim(range(c(fits$psi)))
  }
  else {
    fits %>%
      filter(method == "unpenalized", sample_size == target_sample_size) %>%
      ggplot(aes(x = true_psi, y = reorder(group, true_psi))) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = "Unpenalized\nEstimate"), position = position_nudge(y = -nudge), height = 0, alpha = 0.3) +
      geom_point(aes(x = psi, color = "Unpenalized\nEstimate"), position = position_nudge(y = -nudge),  alpha = 0.3) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = "Penalized\nEstimate"), position = position_nudge(y = nudge), height = 0, alpha = 1, data = fits %>% filter(method == target_method, sample_size == target_sample_size)) +
      geom_point(aes(x = psi, color = "Penalized\nEstimate"), position = position_nudge(y = nudge), alpha = 1, data = fits %>% filter(method == target_method, sample_size == target_sample_size)) +
      geom_point(aes(color = "Truth"), alpha = 1, shape = 18, size = 2.5) +
      #geom_segment(aes(x = unpenalized, xend = .data[[target_method]]), 
      #             position = position_nudge(y = nudge),
      #             arrow = arrow(length = unit(0.1, "cm"), ends = "last", type = "closed"),
      #             data = fits %>% filter(sample_size == target_sample_size) %>% select(group, method, psi, true_psi) %>% pivot_wider(names_from = "method", values_from = "psi")) +
      labs(y = "Group", x = "Group-Specific\nAverage Treatment Effect") +
      xlim(range(c(fits$psi, fits$lower, fits$upper))) +
      scale_color_manual(values = colors, breaks = names(colors)) +
      #xlim(range(c(fits$psi))) + 
      my_theme
  }
} 

G <- 10
SL.library <- c("SL.glm", "SL.glm.interaction", "SL.glmnet")

seed <- 5
data_small  <- simulate(seed = seed, N = G * 25, G = G, beta = 1, theta = 1, sigma = 5)
data_medium <- simulate(seed = seed, N = G * 70, G = G, beta = 1, theta = 1, sigma = 5)
data_large  <- simulate(seed = seed, N = G * 200, G = G, beta = 1, theta = 1, sigma = 5)

fits <- bind_rows(
  estimator(data_small, G = G, SL.library) %>% mutate(sample_size = "small"),
  estimator(data_medium, G = G, SL.library) %>% mutate(sample_size = "medium"),
  estimator(data_large, G = G, SL.library) %>% mutate(sample_size = "large")
)

fits %>% filter(method == "l2")

mse <- fits %>%
  group_by(method, sample_size) %>%
  summarize(mse = mean((true_psi - psi)^2),
            error = mean(true_psi - psi))

mse %>%
  filter(sample_size == "small")

my_theme <- theme(
  axis.text.y = element_blank(), 
  axis.ticks.y = element_blank(),
  legend.position = "right",
  legend.title = element_blank()
)
nudge <- 0.25

p1 <- plot_sample_size("small") + labs(x = "", y = "") + theme(legend.position = "none") +
  ggtitle("Small sample size")
p2 <- plot_sample_size("medium") + labs(x = "") + theme(legend.position = "right") +
  ggtitle("Medium sample size")
p3 <- plot_sample_size("large") + labs(y = "") + theme(legend.position = "none") +
  ggtitle("Large sample size")

p1 / p2 / p3

ggsave("plots/group_effects_example.pdf", width = 10, height = 6)

p1 <- plot_method("unpenalized", "small") + ggtitle("Unpenalized") + labs(x = "") + theme(legend.position = "none")
p2 <- plot_method("unpenalized", "medium") + labs(x = "") + theme(legend.position = "none")
p3 <- plot_method("unpenalized", "large") + theme(legend.position = "none")

p1_l1 <- plot_method("l1", "small") + ggtitle("L1 Penalty") + labs(x = "", y = "") + theme(legend.position = "none")
p2_l1 <- plot_method("l1", "medium") + labs(x = "", y = "") + theme(legend.position = "none")
p3_l1 <- plot_method("l1", "large") + labs(y = "") + theme(legend.position = "none")

p1_l2 <- plot_method("l2", "small") + ggtitle("L2 Penalty") + labs(x = "", y = "")
p2_l2 <- plot_method("l2", "medium") + labs(x = "", y = "") + theme(legend.position = "none")
p3_l2 <- plot_method("l2", "large") + labs(y = "") + theme(legend.position = "none")

#(p1 / p2 / p3) | 
#  (p1_l1 / p2_l1 / p3_l1) |
#  (p1_l2 / p2_l2 / p3_l2)

p1 | p1_l1 | p1_l2


results <- estimator(data_small, G = G, SL.library)
results |> filter(method %in% c("l2", "empirical_bayes")) %>%
  ggplot(aes(x = se^2, y = shrinkage, color = method)) +
  geom_point()
