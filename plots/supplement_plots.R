library(tidyverse)
library(magrittr)
library(patchwork)
library(RColorBrewer)
library(ggsignif)
library(ggh4x)
library(ggdist)

load("analysis/allAnalyses.RData")

# Experiment 1A----
## Job, pet, pronoun----
exp1a_p_allMem <- read.csv("data/exp1a_data.csv", stringsAsFactors = TRUE) %>%
  filter(Task == "memory") %>%
  group_by(SubjID, M_Type, Pronoun) %>%
  summarise(M_Acc_Subj = mean(M_Acc)) %>% # by-subject means
  ggplot(aes(x = M_Type, y = M_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  geom_point(
    position = position_jitterdodge(
      dodge.width = 0.9, jitter.width = 0.5, jitter.height = 0.01, seed = 1
    ),
    size = 0.5, key_glyph = "rect"  # make legend full saturation colors
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    position = position_dodge(width = 0.9),
    alpha = 0.4, color = NA
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    position = position_dodge(width = 0.9),
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  scale_color_brewer(palette = "Dark2", guide = guide_none()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  theme_classic() +
  theme(
    axis.text.x   = element_text(size = 11),
    axis.text.y   = element_text(size = 9),
    axis.ticks.x  = element_blank(),
    axis.title    = element_text(size = 11),
    legend.margin = margin(l = -10),
    legend.text   = element_text(size = 11),
    plot.title    = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(
    title = "Experiment 1: All Memory Questions",
    x     = "Question Type",
    y     = "By-Participant Mean Accuracy",
    fill  = "Character\nPronouns"
  )
exp1a_p_allMem

ggsave(
  plot = exp1a_p_allMem, path = "plots", filename = "sup_exp1a_allMemory.png",
  width = 6.5, height = 4, unit = "in", device = "png"
)

# Experiment 1B----
# Comparing Experiments 1A & 1B----
# By-participant means
exp1_d_subj <- exp1_d %>%
  group_by(Experiment, SubjID, Pronoun) %>%
  summarise(
    M_Acc_Subj    = mean(M_Acc),
    P_Acc_Subj    = mean(P_Acc),
    Diff_Acc_Subj = M_Acc_Subj - P_Acc_Subj
  ) %>%
  mutate(Experiment = str_remove(Experiment, "Exp"))

# Plot labels
exp1_d$M_Acc_Factor %<>% factor(
  labels = c("Memory\nIncorrect", "Memory\nCorrect")
)
exp1_d$Experiment %<>% factor(
  labels = c("1A", "1B")
)

# Plot themes
exp1ab_theme <- theme(
  text             = element_text(size = 11),
  axis.text.x      = element_text(size = 11),
  axis.text.y      = element_text(size = 9),
  axis.ticks.x     = element_blank(),
  axis.title       = element_text(size = 11),
  legend.text      = element_text(size = 11),
  plot.title       = element_text(size = 11, face = "bold"),
  strip.background = element_rect(color = NA),
  strip.clip       = "off",
  strip.placement  = "outside",
  strip.text       = element_text(size = 10, margin = margin(0, 0, 0, 0))
)

## Just they/them----
#### Memory----
exp1ab_p_they_memory <- ggplot(
  data = exp1_d_subj %>% filter(Pronoun == "they/them"),
  aes(x = Experiment, y = M_Acc_Subj, color = Experiment, fill = Experiment)) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    position = position_dodge(width = 0.5),
    color = NA, alpha = 0.4
  ) +
  geom_point(
    position = position_jitter(width = 0.4, height = 0.01, seed = 1),
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    position = position_dodge(width = 0.9),
    linewidth = 0.5, width = 0.5, color = "black"
  ) +
  geom_signif(
    comparisons = list(c("1A", "1B")),
    map_signif_level = TRUE,
    color = "black", tip_length = 0, textsize = 3, vjust = -0.2
  ) +
  scale_color_manual(
    values = c("grey50", "#7570B3"),
    labels = c("1A:\nMemory\nFirst", "1B:\nProduction\nFirst")
  ) +
  scale_fill_manual(
    values = c("grey50", "#7570B3"),
    labels = c("1A:\nMemory\nFirst", "1B:\nProduction\nFirst")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(-0.01, 1.15),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  theme_classic() +
  exp1ab_theme +
  guides(
    color = guide_none(),
    fill  = guide_legend(byrow = TRUE)
  ) +
  labs(
    title = "Memory",
    x     = element_blank(),
    y     = "Mean Accuracy By Participant",
    fill  = "Experiment"
  )

#### Production----
exp1ab_p_they_production <- ggplot(
  data = exp1_d_subj %>% filter(Pronoun == "they/them"),
  aes(x = Experiment, y = P_Acc_Subj, color = Experiment, fill = Experiment)) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    position = position_dodge(width = 0.5),
    color = NA, alpha = 0.4
  ) +
  geom_point(
    position = position_jitter(width = 0.4, height = 0.01, seed = 1),
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    position = position_dodge(width = 0.9),
    linewidth = 0.5, width = 0.5, color = "black"
  ) +
  geom_signif(
    comparisons = list(c("1A", "1B")),
    map_signif_level = TRUE,
    color = "black", tip_length = 0, textsize = 3, vjust = -0.2
  ) +
  scale_color_manual(
    values = c("grey50", "#7570B3"),
    labels = c("1A:\nMemory\nFirst", "1B:\nProduction\nFirst")
  ) +
  scale_fill_manual(
    values = c("grey50", "#7570B3"),
    labels = c("1A:\nMemory\nFirst", "1B:\nProduction\nFirst")
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    expand = c(0, 0), limits = c(-0.01, 1.15),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  theme_classic() +
  exp1ab_theme +
  guides(
    color = guide_none(),
    fill  = guide_legend(byrow = TRUE)
  ) +
  labs(
    title = "Production",
    x     = element_blank(),
    y     = "Mean Accuracy By Participant",
    fill  = "Experiment"
  )

#### Production by memory----
exp1ab_p_they_both <- ggplot(
  data = exp1_d %>% filter(Pronoun == "they/them"),
  aes(x = interaction(M_Acc_Factor, Experiment), y = P_Acc,
      fill = Experiment, alpha = M_Acc_Factor)) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    position = position_dodge(), color = NA
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    position = position_dodge(width = 0.9), key_glyph = "rect",
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  geom_signif(
    xmin = c(1, 3, 1.5), xmax = c(2, 4, 3.5), y = c(0.75, 0.75, 0.9),
    annotations = c("***", "***", "NS."),
    color = "black", alpha = 1, tip_length = 0, textsize = 3,  vjust = -0.2
  ) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_manual(values = c("grey50", "#7570B3")) +
  scale_x_discrete(expand = c(0, 0), guide = "axis_nested") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme_classic() +
  exp1ab_theme +
  theme(ggh4x.axis.nestline = element_line(linetype = 0)) +
  guides(alpha = guide_none(), fill = guide_none()) +
  labs(
    title = "Production Split By Memory",
    x     = element_blank(),
    y     = "Production Accuracy"
  )

#### Combine----
exp1ab_p_they <-  exp1ab_p_they_memory + exp1ab_p_they_production +
  exp1ab_p_they_both + guide_area() +
  plot_layout(
    guides = "collect",
    design = "AABB
              CCCD") +
  plot_annotation(
    title = "Experiments 1A & 1B: Accuracy for They/Them Characters",
    theme = theme(plot.title = element_text(size = 12, face = "bold"))
  )
exp1ab_p_they

ggsave(
  plot = exp1ab_p_they, path = "plots", filename = "sup_exp1ab_they.png",
  width = 6.5, height = 6.5, unit = "in", device = "png"
)

## Task difference
exp1ab_p_task <- ggplot(exp1_d_subj,
  aes(x = Diff_Acc_Subj, y = Pronoun, fill = Pronoun,
      alpha = fct_rev(Experiment))
  ) +
  stat_slab(
    normalize = "xy", justification = 0.5,
    position = position_dodge(width = 1)
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "pointrange", key_glyph = "rect",
    position = position_dodgejust(width = 1, justification = 0.05),
    fill = "black", size = 0.5, linewidth = 0.75,
  ) +
  geom_vline(xintercept = 0) +
  scale_alpha_manual(
    values = c(0.5, 1),
    labels = c("1B:\nProduction\nFirst", "1A:\nMemory\nFirst")
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.ticks.y  = element_blank(),
    axis.text.x   = element_text(size = 10),
    axis.text.y   = element_blank(),
    legend.margin = margin(l = 0, r = 0, t = 5, b = 5, unit = "pt"),
    legend.text   = element_text(size = 11),
    legend.title  = element_text(size = 11),
    plot.title    = element_text(size = 12, face = "bold")
  ) +
  guides(alpha = guide_legend(byrow = TRUE, reverse = TRUE)) +
  labs(
    title = "Experiments 1A & 1B: Difference Between Tasks",
    x     = "Production More Accurate – Memory More Accurate",
    y     = element_blank(),
    alpha = "Experiment"
  )
exp1ab_p_task

ggsave(
  plot = exp1ab_p_task, path = "plots", filename = "sup_exp1ab_taskDiff.png",
  width = 6.5, height = 4, unit = "in", device = "png"
)

## All accuracy & distribution----
#### Memory distribution----
exp1ab_p_memory_dist <- ggplot(exp1_d,
  aes(x = Experiment, fill = M_Response, alpha = Experiment)) +
  geom_bar(position = "fill") +
  facet_wrap(~Pronoun, strip.position = "bottom") +
  scale_alpha_manual(
    values = c(1, 1),
    labels = c("1A:\nMemory\nFirst", "1B:\nProduction\nFirst")
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  exp1ab_theme +
  theme(
    axis.text.y = element_text(size = 8),
    axis.ticks  = element_blank()
  ) +
  guides(
    alpha = guide_legend(
      byrow = TRUE, override.aes = theme(fill = NA),
      label.position = "left", label.hjust = 0
    ),
    fill  = guide_none()
  ) +
  labs(
    title = "Memory Distribution",
    x     = element_blank(),
    y     = "Proportion of Trials",
    fill  = "Pronoun\nSelected",
   alpha  = "Experiment")

#### Memory accuracy----
exp1ab_p_memory_acc <- ggplot(exp1_d_subj,
  aes(x = Experiment, y = M_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  stat_summary(fun.data = mean_cl_boot, geom = "bar", alpha = 0.4, color = NA) +
  geom_point(
    position = position_jitter(width = 0.4, height = 0.02, seed = 1),
    size = 0.2
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  facet_wrap(~Pronoun, strip.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  guides(fill = guide_none(), color = guide_none()) +
  theme_classic() +
  exp1ab_theme +
  theme(axis.text.y = element_text(size = 8)) +
  labs(
    title = "Memory Accuracy",
    x     = element_blank(),
    y     = "Mean Accuracy by Participant"
  )

#### Production distribution----
exp1_d$P_Pronoun %<>% fct_relevel("none", after = 4)

exp1ab_p_prod_dist <- ggplot(exp1_d,
  aes(x = Experiment, fill = P_Pronoun)) +
  geom_bar(position = "fill") +
  facet_wrap(~Pronoun, strip.position = "bottom") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  exp1ab_theme +
  theme(
    axis.text.y = element_text(size = 8),
    axis.ticks = element_blank(),
    plot.margin = margin(l = 15)
  ) +
  labs(
    title = "Production Distribution",
    x     = element_blank(),
    y     = "Proportion of Trials",
    fill  = "Pronoun\nSelected"
  )

#### Production accuracy----
exp1ab_p_prod_acc <- ggplot(exp1_d_subj,
  aes(x = Experiment, y = P_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  stat_summary(fun.data = mean_cl_boot, geom = "bar", alpha = 0.4, color = NA) +
  geom_point(
    position = position_jitter(width = 0.4, height = 0.02, seed = 1),
    size = 0.2
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  facet_wrap(~Pronoun, strip.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  guides(fill = guide_none(), color = guide_none()) +
  theme_classic() +
  exp1ab_theme +
  theme(axis.text.y = element_text(size = 8)) +
  labs(
    title = "Production Accuracy",
    x     = element_blank(),
    y     = "Mean Accuracy by Participant"
  )

#### Combine----
exp1ab_p_panel1 <- exp1ab_p_memory_acc + exp1ab_p_memory_dist +
  guides(fill = guide_none()) +
  exp1ab_p_prod_acc + exp1ab_p_prod_dist +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparing Experiments 1A & 1B",
    theme = theme(
      plot.margin = margin(t = 8, b = 2, l = 2, r = -10, unit = "pt"),
      plot.title  = element_text(size = 12, face = "bold"))
  )
exp1ab_p_panel1

ggsave(
  plot = exp1ab_p_panel1, path = "plots", filename = "sup_exp1ab_panel1.png",
  width = 6.5, height = 5.5, unit = "in", device = "png"
)

## Memory -> Production----
#### Combined accuracy----
# Code response matches
exp1_d %<>% mutate(CompareTask = case_when(
  M_Acc == 1 & P_Acc == 1 ~ "Both \nRight",
  M_Acc == 0 & P_Acc == 0 ~ "Both \nWrong",
  M_Acc == 1 & P_Acc == 0 ~ "Memory \nOnly",
  M_Acc == 0 & P_Acc == 1 ~ "Production \nOnly"
))

exp1_d$CompareTask %<>% factor(levels = c(
  "Memory \nOnly", "Production \nOnly", "Both \nWrong", "Both \nRight"
))

exp1ab_p_mp_compare <- ggplot(exp1_d,
  aes(x = Experiment, fill = CompareTask, alpha = Experiment)) +
  geom_bar(position = "fill") +
  facet_wrap(~Pronoun, strip.position = "bottom") +
  scale_alpha_manual(
    values = c(1, 1),
    labels = c("1A:\nMemory First", "1B:\nProduction First")
  ) +
  scale_fill_manual(values = c("pink3", "#E6AB02", "tomato3", "#367ABF")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  exp1ab_theme +
  theme(axis.ticks = element_blank()) +
  guides(
    alpha = guide_legend(
      byrow = TRUE, label.position = "left",
      label.hjust = 0, override.aes = theme(fill = NA)
    ),
    fill  = guide_legend(order = 1)
  ) +
  labs(
    title = "Combined Accuracy",
    x     = element_blank(),
    y     = "Proportion of Characters",
    alpha = "Experiment",
    fill  = "Accuracy\nPattern",
  )

#### Production split by memory----
exp1ab_p_mp_split <- ggplot(exp1_d,
  aes(x = Experiment, y = P_Acc, fill = Pronoun, alpha = factor(M_Acc))) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    position = position_dodge(width = 0.9)
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    position = position_dodge(width = 0.9),
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  scale_alpha_discrete(
    range  = c(0.5, 1),
    labels = c("Memory\nIncorrect", "Memory\nCorrect")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  facet_wrap(~Pronoun, strip.position = "bottom") +
  theme_classic() +
  exp1ab_theme +
  guides() +
  guides(
    alpha = guide_legend(byrow = TRUE, override.aes = theme(color = NA)),
    fill = guide_none()
  ) +
  labs(
    title = "Production Accuracy\nSplit By Memory Accuracy",
    x     = element_blank(),
    y     = "Production Accuracy",
    alpha = "Memory\nAccuracy",
  )

#### Combine----
exp1ab_p_panel2 <- exp1ab_p_mp_compare / exp1ab_p_mp_split +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparing Experiments 1A & 1B",
    theme = theme(
      plot.margin = margin(t = 8, b = 2, l = 2, r = -25, unit = "pt"),
      plot.title = element_text(size = 12, face = "bold")
    )
  )
exp1ab_p_panel2

ggsave(
  plot = exp1ab_p_panel2, path = "plots", filename = "sup_exp1ab_panel2.png",
  width = 5.5, height = 5.5, unit = "in", device = "png"
)

# Experiment 2----
exp2_d$P_Response %<>% fct_relevel("none", after = 4)  # put in correct order

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

## Distributions----
#### Memory----
exp2_p_memory_dist <- ggplot(exp2_d, aes(x = Pronoun, fill = M_Response)) +
  geom_bar(position = "fill") +
  facet_grid(fct_rev(Biographies) ~ fct_rev(PSA), labeller = labeller(
    `fct_rev(PSA)` = c("1" = "Gendered Language PSA", "0" = "Unrelated PSA"),
    `fct_rev(Biographies)` = c("1" = "They Bios", "0" = "He/She Bios"))
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme_classic() +
  exp2_theme +
  theme(
    axis.line     = element_blank(),
    axis.ticks    = element_blank(),
    panel.border  = element_rect(fill = NA, color = "black", linewidth = 0.5),
    panel.spacing = unit(0.15, "in")
  ) +
  labs(
    title = "Memory",
    x     = "Correct Pronoun",
    y     = "Proportion of Trials",
    fill  = "Pronoun\nSelected"
  )

#### Production----
exp2_p_prod_dist <- ggplot(exp2_d, aes(x = Pronoun, fill = P_Response)) +
  geom_bar(position = "fill") +
  facet_grid(fct_rev(Biographies) ~ fct_rev(PSA), labeller = labeller(
    `fct_rev(PSA)` = c("1" = "Gendered Language PSA", "0" = "Unrelated PSA"),
    `fct_rev(Biographies)` = c("1" = "They Bios", "0" = "He/She Bios"))
  ) +
  scale_fill_brewer(palette = "Dark2") +
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
  labs(
    title = "Production",
    x     = "Correct Pronoun",
    y     = "Proportion of Trials",
    fill  = "Pronoun\nProduced"
  )

#### Combine----
exp2_p_dist <- exp2_p_memory_dist / exp2_p_prod_dist +
  plot_annotation(
    title = "Experiment 2: Distribution of Responses",
    tag_levels = "A",
    theme = theme(
      plot.margin = margin(t = 8, b = 2, l = 2, r = 0, unit = "pt"),
      plot.title = element_text(size = 12, face = "bold")
    )
  )
exp2_p_dist

ggsave(
  plot = exp2_p_dist, path = "plots", filename = "sup_exp2_distributions.png",
  width = 6.5, height = 6.75, unit = "in", device = "png"
)

## Job, pet, pronoun----
exp2_p_allMem <- exp2_d_all %>%
  group_by(Participant, M_Type, Pronoun) %>%
  summarise(M_Acc_Subj = mean(M_Acc)) %>% # by-subject means
  ggplot(aes(x = M_Type, y = M_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  geom_point(
    position = position_jitterdodge(
      dodge.width = 0.9, jitter.width = 0.5, jitter.height = 0.02, seed = 1
    ),
    size = 0.25, key_glyph = "rect"  # make legend full saturation colors
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    position = position_dodge(width = 0.9),
    alpha = 0.4, color = NA
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    position = position_dodge(width = 0.9),
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  scale_color_brewer(palette = "Dark2", guide = guide_none()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  theme_classic() +
  theme(
    axis.text.x   = element_text(size = 11),
    axis.text.y   = element_text(size = 9),
    axis.ticks.x  = element_blank(),
    axis.title    = element_text(size = 11),
    legend.margin = margin(l = -10),
    legend.text   = element_text(size = 11),
    plot.title    = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Experiment 2: All Memory Questions",
    x     = "Question Type",
    y     = "By-Participant Mean Accuracy",
    fill  = "Character\nPronouns"
  )
exp2_p_allMem

ggsave(
  plot = exp2_p_allMem, path = "plots", filename = "sup_exp2_allMemory.png",
  width = 6.5, height = 4, unit = "in", device = "png"
)
