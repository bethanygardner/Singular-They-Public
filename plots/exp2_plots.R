library(tidyverse)
library(magrittr)
library(patchwork)
library(RColorBrewer)

load("all_analyses.RData")

exp2_theme <- theme(
  axis.text.x      = element_text(size = 11),
  axis.text.y      = element_text(size = 9),
  axis.title       = element_text(size = 11),
  legend.text      = element_text(size = 11),
  plot.tag         = element_text(size = 12, face = "bold"),
  plot.title       = element_text(size = 11, face = "bold"),
  strip.text       = element_text(size = 11),
  strip.background = element_rect(fill = "grey90", linewidth = 0.5)
)

# Memory----
## Accuracy----
exp2_p_memory_acc <- ggplot(
  data = exp2_d %>%
    group_by(Participant, PSA, Biographies, Pronoun) %>%
    summarise(M_Acc_Subj = mean(M_Acc)
  ),
  aes(x = Pronoun, y = M_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  stat_summary(fun.data = mean_cl_boot, geom = "bar", alpha = 0.4, color = NA) +
  geom_point(
    position = position_jitter(height = 0.01, width = 0.35, seed = 2),
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  facet_grid(fct_rev(Biographies) ~ fct_rev(PSA), labeller = labeller(
    `fct_rev(PSA)` = c("1" = "Gendered Language PSA", "0" = "Unrelated PSA"),
    `fct_rev(Biographies)` = c("1" = "They Bios", "0" = "He/She Bios")
  )) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  exp2_theme +
  theme(
    axis.line    = element_blank(),
    axis.ticks   = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
   ) +
  guides(fill = guide_none(), color = guide_none()) +
  labs(
    x = element_blank(),
    y = "By-Participant Mean Accuracy"
  )

## PSA effect----
exp2_p_memory_PSA <- exp2_d %>%
  mutate(Pronoun_Group = ifelse(
    Pronoun == "they/them", "they/them", "he/him +\nshe/her"
  )) %>%
  group_by(PSA, Biographies, Participant, Pronoun_Group) %>%
  summarise(M_Acc = mean(M_Acc)) %>%  # summarise across he + she
  group_by(PSA, Biographies, Pronoun_Group) %>%
  summarise(mean_se(M_Acc)) %>%  # summarise across participants
  mutate(Condition = str_c(PSA, Biographies, sep = " + ")) %>%
  ggplot(aes(
    x = Pronoun_Group, y = y, ymin = ymin, ymax = ymax,
    group = Condition, color = fct_rev(PSA)
  )) +
  geom_line(aes(linetype = fct_rev(Biographies))) +
  geom_pointrange(size = 0.25) +
  scale_color_manual(
    labels = c("0" = "Unrelated", "1" = "Gendered\nLanguage"),
    values = c("tomato3", "#367ABF")
  ) +
  scale_linetype_discrete(labels = c("1" = "They", "0" = "He/She")) +
  scale_x_discrete(expand = c(0.06, 0.06)) +
  theme_classic() +
  exp2_theme +
  theme(
    axis.ticks.x = element_blank(),
    legend.box   = "horizontal"
  ) +
  labs(
    x        = element_blank(),
    y        = "Mean Accuracy",
    color    = "PSA",
    linetype = "Biography"
  )

## Combine----
exp2_p_memory <- exp2_p_memory_acc + exp2_p_memory_PSA +
  plot_annotation(
    title      = "Experiment 2: Accuracy of Memory Responses",
    tag_levels = "A",
    theme      = theme(
      plot.margin = margin(t = 8, b = -5, l = 5, r = 5, unit = "pt"),
      plot.title  = element_text(size = 12, face = "bold")
    )
  ) +
  plot_layout(
    design = "AAAAAAA
              BBBBBB#",
    heights = c(2, 1.5)
  )
exp2_p_memory

ggsave(
  plot = exp2_p_memory, path = "plots", filename = "exp2_memory.png",
  width = 6.5, height = 5.5, unit = "in", device = "png"
)

# Production----
## Accuracy----
exp2_p_prod_acc <- ggplot(
  data = exp2_d %>%
    group_by(Participant, PSA, Biographies, Pronoun) %>%
    summarise(P_Acc_Subj = mean(P_Acc)
  ),
  aes(x = Pronoun, y = P_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  stat_summary(fun.data = mean_cl_boot, geom = "bar", alpha = 0.4, color = NA) +
  geom_point(
    position = position_jitter(height = 0.01, width = 0.35, seed = 2),
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  facet_grid(fct_rev(Biographies) ~ fct_rev(PSA), labeller = labeller(
    `fct_rev(PSA)` = c("1" = "Gendered Language PSA", "0" = "Unrelated PSA"),
    `fct_rev(Biographies)` = c("1" = "They Bios", "0" = "He/She Bios")
  )) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  theme_classic() +
  exp2_theme +
  theme(
    axis.line    = element_blank(),
    axis.ticks   = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
  ) +
  guides(fill = guide_none(), color = guide_none()) +
  labs(
    x = element_blank(),
    y = "By-Participant Mean Accuracy"
  )

## PSA effect----
exp2_p_prod_PSA <- exp2_d %>%
  mutate(Pronoun_Group = ifelse(
    Pronoun == "they/them", "they/them", "he/him +\nshe/her")
  ) %>%
  group_by(PSA, Biographies, Participant, Pronoun_Group) %>%
  summarise(P_Acc = mean(P_Acc)) %>%  # summarize across he + she
  group_by(PSA, Biographies, Pronoun_Group) %>%
  summarise(mean_se(P_Acc)) %>%  # summarize across participants
  mutate(Condition = str_c(PSA, Biographies, sep = " + ")) %>%
  ggplot(aes(
    x = Pronoun_Group, y = y, ymin = ymin, ymax = ymax,
    group = Condition, color = fct_rev(PSA)
  )) +
  geom_line(aes(linetype = fct_rev(Biographies))) +
  geom_pointrange(size = 0.25) +
  scale_color_manual(
    labels = c("0" = "Unrelated", "1" = "Gendered\nLanguage"),
    values = c("tomato3", "#367ABF")
  ) +
  scale_linetype_discrete(labels = c("1" = "They", "0" = "He/She")) +
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  exp2_theme +
  theme(
    axis.ticks.x = element_blank(),
    legend.box.margin = margin(l = -10, unit = "pt")
  ) +
  labs(
    x        = element_blank(),
    y        = "Mean Accuracy",
    color    = "PSA",
    linetype = "Biography"
  )

## Produce they----
# Count how many times each participant produced they/them
exp2_d_they <- exp2_d %>%
  mutate(P_IsThey = ifelse(P_Response == "they/them", 1, 0)) %>%
  group_by(Participant, PSA, Biographies) %>%
  summarize(P_Count = sum(P_IsThey)) %>%
  mutate(Dummy = "")

# Group for plots
exp2_d_they$P_Count %<>% as.factor() %>%
  recode(
    "6" = "6+", "7" = "6+", "8" = "6+", "9" = "6+",
    "10" = "6+", "11" = "6+", "12" = "6+"
  )

exp2_p_prod_they <- ggplot(exp2_d_they, aes(x = Dummy, fill = P_Count)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#666666", brewer.pal(6, "Purples"))) +
  facet_grid(fct_rev(Biographies) ~ fct_rev(PSA), labeller = labeller(
    `fct_rev(PSA)` = c("1" = "Gendered\nLang. PSA", "0" = "Unrelated \nPSA"),
    `fct_rev(Biographies)` = c("1" = "They Bios", "0" = "He/She Bios")
  )) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  exp2_theme +
  theme(
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    axis.title.x  = element_text(margin = margin(t = -20, unit = "pt")),
    legend.margin = margin(0, 0, 0, 0),
    panel.border  = element_rect(fill = NA, color = "black", linewidth = 0.5),
    panel.spacing = unit(0.15, "in")
  ) +
  labs(
    x    = "Number of They/Them \nResponses per Participant",
    y    = "Proportion of Participants",
    fill = element_blank()
  )

## Combine----
exp2_p_prod <- exp2_p_prod_acc + exp2_p_prod_PSA + exp2_p_prod_they +
  plot_annotation(
    title = "Experiment 2: Accuracy & Distribution of Production Responses",
    tag_levels = "A",
    theme = theme(
      plot.margin = margin(t = 8, b = 0, l = 2, r = 2, unit = "pt"),
      plot.title  = element_text(size = 12, face = "bold")
    )
  ) +
  plot_layout(
    design = "AAAAA
              BBCCC"
  )
exp2_p_prod

ggsave(
  plot = exp2_p_prod, path = "plots", filename = "exp2_production.png",
  width = 6.5, height = 6.5, unit = "in", device = "png"
)

# Memory -> Production----
## Combined accuracy----
# Code response matches
exp2_d %<>% mutate(CompareTask = case_when(
  M_Acc == 1 & P_Acc == 1 ~ "Both \nRight",
  M_Acc == 0 & P_Acc == 0 ~ "Both \nWrong",
  M_Acc == 1 & P_Acc == 0 ~ "Memory \nOnly",
  M_Acc == 0 & P_Acc == 1 ~ "Production \nOnly"
))

exp2_d$CompareTask %<>% factor(levels = c(
  "Memory \nOnly", "Production \nOnly", "Both \nWrong", "Both \nRight"
))

exp2_p_compare <- ggplot(exp2_d, aes(x = Pronoun, fill = CompareTask)) +
  geom_bar(position = "fill") +
  facet_grid(fct_rev(Biographies) ~ fct_rev(PSA), labeller = labeller(
    `fct_rev(PSA)` = c("1" = "Gendered Language PSA", "0" = "Unrelated PSA"),
    `fct_rev(Biographies)` = c("1" = "They Bios", "0" = "He/She Bios")
  )) +
  scale_fill_manual(values = c("pink3", "#E6AB02", "tomato3", "#367ABF")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  exp2_theme +
  theme(
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    panel.border  = element_rect(fill = NA, color = "black", linewidth = 0.5),
    panel.spacing = unit(0.15, "in")
  ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(
    title = "Combined Accuracy",
    x     = element_blank(),
    y     = "Proportion of Characters",
    fill  = element_blank(),
  )

## Production split by memory----
exp2_p_split <- ggplot(exp2_d,
  aes(x = Pronoun, y = P_Acc, fill = Pronoun, alpha = factor(M_Acc))) +
  stat_summary(fun.data = mean_cl_boot, geom = "bar", position = "dodge") +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    position = position_dodge(0.9), width = 0.5, linewidth = 0.5
  ) +
  facet_grid(fct_rev(Biographies) ~ fct_rev(PSA), labeller = labeller(
    `fct_rev(PSA)` = c("1" = "Gendered Language PSA", "0" = "Unrelated PSA"),
    `fct_rev(Biographies)` = c("1" = "They Bios", "0" = "He/She Bios")
  )) +
  scale_alpha_discrete(
    range  = c(0.5, 1),
    labels = c("Memory\nIncorrect", "Memory\nCorrect")
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme_classic() +
  exp2_theme +
  theme(
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    panel.border  = element_rect(fill = NA, color = "black", linewidth = 0.5),
    panel.spacing = unit(0.15, "in")
  ) +
  guides(
    alpha = guide_legend(byrow = TRUE, override.aes = theme(color = NA)),
    fill = guide_none()
  ) +
  labs(
    title = "Production Split By Memory Accuracy",
    x     = element_blank(),
    y     = "Production Accuracy",
    alpha = element_blank(),
  )

## Combine----
exp2_p_both <- exp2_p_split / exp2_p_compare +
  plot_annotation(
    title = "Experiment 2: Memory & Production",
    tag_levels = "A",
    theme = theme(
      plot.margin = margin(t = 8, b = 5, l = 5, r = -5, unit = "pt"),
      plot.title = element_text(size = 12, face = "bold")
    )
  )
exp2_p_both

ggsave(
  plot = exp2_p_both, path = "plots", filename = "exp2_both.png",
  width = 6.5, height = 6.5, unit = "in", device = "png"
)
