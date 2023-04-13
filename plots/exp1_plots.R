library(tidyverse)
library(magrittr)
library(patchwork)
library(RColorBrewer)

load("analysis/allAnalyses.RData")

# Memory----
## Accuracy----
exp1_p_mem_acc <- exp1a_d %>%
  group_by(SubjID, Pronoun) %>%
  summarise(M_Acc_Subj = mean(M_Acc)) %>%
  ggplot(aes(x = Pronoun, y = M_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  geom_point(
    position = position_jitter(width = 0.4, height = 0.01, seed = 1),
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    alpha = 0.4, color = NA
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  guides(fill = guide_none(), color = guide_none()) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 9),
    axis.ticks  = element_blank(),
    axis.title  = element_text(size = 11),
    plot.tag    = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Pronoun",
    y = "By-Participant Mean Accuracy"
  )

## Distribution----
exp1_p_mem_dist <- ggplot(exp1a_d,
  aes(x = Pronoun, fill = M_Response)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.text.x   = element_text(size = 11),
    axis.text.y   = element_text(size = 9),
    axis.ticks  = element_blank(),
    axis.title    = element_text(size = 11),
    legend.margin = margin(l = 0, r = -5),
    legend.text   = element_text(size = 11),
    plot.tag      = element_text(size = 12, face = "bold")
  ) +
  labs(
    x    = "Correct Pronoun",
    y    = "Proportion of Trials",
    fill = "Pronoun\nSelected"
)

## Combined----
exp1_p_mem <-  exp1_p_mem_acc + exp1_p_mem_dist +
  plot_annotation(
    title = "Experiment 1: Accuracy & Distribution of Memory Responses",
    tag_levels = "A",
    theme = theme(
      plot.margin = margin(t = 5, b = 0, l = 2, r = 5, unit = "pt"),
      plot.title = element_text(face = "bold", size = 12)
    )
  )
exp1_p_mem

ggsave(
  plot = exp1_p_mem, path = "plots", filename = "exp1_memory.png",
  width = 6.5, height = 3.25, unit = "in", device = "png"
)

# Production----
## Accuracy----
exp1_p_prod_acc <- exp1a_d %>%
  group_by(SubjID, Pronoun) %>%
  summarise(P_Acc_Subj = mean(M_Acc)) %>%
  ggplot(aes(x = Pronoun, y = P_Acc_Subj, fill = Pronoun, color = Pronoun)) +
  geom_point(
    position = position_jitter(width = 0.4, height = 0.01, seed = 1),
    size = 0.5
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "bar",
    alpha = 0.4, color = NA
  ) +
  stat_summary(
    fun.data = mean_cl_boot, geom = "errorbar",
    color = "black", linewidth = 0.5, width = 0.5
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  guides(fill = guide_none(), color = guide_none()) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 9),
    axis.ticks  = element_blank(),
    axis.title  = element_text(size = 11),
    plot.tag    = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = element_blank(),
    y = "By-Participant Mean Accuracy"
  )

## Distribution----
exp1a_d$P_Pronoun %<>% fct_relevel("none", after = 4)  # put in correct order

exp1_p_prod_dist <- ggplot(exp1a_d,
  aes(x = Pronoun, fill = P_Pronoun)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.text.x   = element_text(size = 11, angle = 15, vjust = .65),
    axis.text.y   = element_text(size = 9),
    axis.ticks    = element_blank(),
    axis.title    = element_text(size = 11),
    legend.margin = margin(l = 0, r = -5),
    legend.text   = element_text(size = 11),
    plot.tag      = element_text(size = 12, face = "bold")
  ) +
  labs(
    x    = "Correct Pronoun",
    y    = "Proportion of Trials",
    fill = "Pronoun\nProduced"
  )

## Use they/them----
# Count how many times each participant produced they/them
exp1a_d_they <- exp1a_d %>%
  mutate(P_IsThey = ifelse(P_Pronoun == "they/them", 1, 0)) %>%
  group_by(SubjID) %>%
  summarize(P_Count = sum(P_IsThey)) %>%
  mutate(Dummy = "")

# Group for plots
exp1a_d_they$P_Count %<>% as.factor() %>%
  recode(
    "6" = "6+", "7" = "6+", "8" = "6+", "9" = "6+",
    "10" = "6+", "11" = "6+", "12" = "6+"
  )

exp1_p_prod_they <- ggplot(exp1a_d_they,
                      aes(x = Dummy, fill = P_Count)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#666666", brewer.pal(6, "Purples"))) +
  scale_x_discrete(expand =  c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.text.x   = element_text(size = 11),
    axis.text.y   = element_text(size = 9),
    axis.ticks    = element_blank(),
    axis.title    = element_text(size = 11),
    axis.title.x  = element_text(margin = margin(t = -10, unit = "pt")),
    legend.margin = margin(l = 0, r = -5),
    legend.text   = element_text(size = 11),
    plot.tag      = element_text(size = 12, face = "bold")
  ) +
  labs(
    x    = "Number of They/Them \nResponses per Participant",
    y    = "Proportion of Participants",
    fill = element_blank()
  )

## Combined----
exp1_p_prod <- exp1_p_prod_acc / (exp1_p_prod_dist | exp1_p_prod_they) +
  plot_annotation(
    title = "Experiment 1: Accuracy & Distribution of Production Responses",
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(face = "bold", size = 12))
  )
exp1_p_prod

ggsave(
  plot = exp1_p_prod, path = "plots", filename = "exp1_production.png",
  width = 6.5, height = 6.5, unit = "in", device = "png"
)

# Memory -> Production----
## Compare accuracy----
# Code response matches
exp1a_d %<>% mutate(CompareTask = case_when(
  M_Acc == 1 & P_Acc == 1 ~ "Both \nRight",
  M_Acc == 0 & P_Acc == 0 ~ "Both \nWrong",
  M_Acc == 1 & P_Acc == 0 ~ "Memory \nOnly",
  M_Acc == 0 & P_Acc == 1 ~ "Production \nOnly"
))

exp1a_d$CompareTask %<>% factor(levels = c(
  "Memory \nOnly", "Production \nOnly", "Both \nWrong", "Both \nRight"
))

exp1_p_mp_compare <- ggplot(exp1a_d,
  aes(x = Pronoun, fill = CompareTask)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("pink3", "#E6AB02", "tomato3", "#367ABF")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.text.x   = element_text(size = 11, angle = 15, vjust = 0.65),
    axis.text.y   = element_text(size = 9),
    axis.ticks    = element_blank(),
    legend.margin = margin(l = 0),
    legend.text   = element_text(size = 11),
    plot.tag      = element_text(size = 12, face = "bold"),
    plot.title    = element_text(size = 11, face = "bold")
  ) +
  guides(fill = guide_legend(byrow = TRUE, keywidth = 0.75)) +
  labs(
    title = "Combined Accuracy",
    x     = element_blank(),
    y     = "Proportion of Characters",
    fill  = element_blank())

## Production split by memory----
exp1_p_mp_split <- ggplot(exp1a_d,
  aes(x = Pronoun, y = P_Acc, fill = Pronoun, alpha = factor(M_Acc))) +
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
  guides(fill = "none", color = "none") +
  theme_classic() +
  theme(
    axis.text.x   = element_text(size = 11, angle = 15, vjust = 0.65),
    axis.text.y   = element_text(size = 9),
    axis.ticks    = element_blank(),
    legend.margin = margin(l = 0),
    legend.text   = element_text(size = 11),
    plot.tag      = element_text(size = 12, face = "bold"),
    plot.title    = element_text(size = 11, face = "bold")
  ) +
  guides(alpha = guide_legend(
    byrow = TRUE, keywidth = 0.75,
    override.aes = theme(color = NA))
  ) +
  labs(
    title = "Production Accuracy\nSplit By Memory Accuracy",
    x     = element_blank(),
    y     = "Production Accuracy",
    alpha = element_blank()
  )

## Combined----
exp1_p_mp <- exp1_p_mp_split + exp1_p_mp_compare +
  plot_annotation(
    title = "Experiment 1: Memory & Production",
    tag_levels = "A",
    theme = theme(
      plot.margin = margin(t = 5, b = -5, l = 2, r = 2, unit = "pt"),
      plot.title  = element_text(face = "bold", size = 12)
    )
  )
exp1_p_mp

ggsave(
  plot = exp1_p_mp, path = "plots", filename = "exp1_both.png",
  width = 6.5, height = 3.25, unit = "in", device = "png"
)
