
# Duration in RP 5 --------------------------------------------------------
# 1st rat to complete both the alone and 50-300ms duration allowing for direct comparison.
# After discussion w/ Ben on 8/23/22, decided to try 

# Rxn_overall_by_Duration %>%
#   filter(!(Intensity %in% c("90"))) %>%
#   filter(ID %in% c("RP 5")) %>%
#   mutate(Genotype = str_extract(Genotype, "Het|WT"),
#          `Stim Length (ms)` = as.factor(`Dur (ms)`)) %>%
#   ggplot(aes(x = Intensity, y = Rxn)) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                fun.min = function(x) mean(x) - se(x),
#                fun.max = function(x) mean(x) + se(x),
#                geom = "errorbar", width = 1, position = position_dodge(0.1)) +
#   stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                geom = "point", position = position_dodge(0.1), size = 3) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
#   labs(title = "Presentation of single or mixed duration stimuli for RP 5",
#        caption = paste("Date:", head(df$Date)),
#        x = "Intensity (dB)",
#        y = "Mean Reaction time (ms)") +
#   scale_x_continuous(breaks = seq(20, 80, by = 10)) +
#   facet_wrap( ~ Duration, ncol = 1) +
#   theme_classic() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
#   )

Rxn_overall_by_Duration %>%
  filter(!(Intensity %in% c("90"))) %>%
  filter(ID %in% c("RP 5")) %>%
  mutate(Genotype = str_extract(Genotype, "Het|WT"),
         `Stim Length (ms)` = as.factor(`Dur (ms)`),
         Phase =  case_when(Phase == "BBN Rotating" ~ "Rotating",
                            TRUE ~ "")) %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
  labs(title = "Presentation of single or mixed duration stimuli for RP 5",
       caption = paste("Date:", head(df$Date)),
       x = "Intensity (dB)",
       y = "Mean Reaction time (ms)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  facet_wrap(Duration ~ Phase, ncol = 2) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )

# Duration in RP 3 --------------------------------------------------------
# 1st rat to complete both the alone and 50-300ms duration allowing for direct comparison.
# After discussion w/ Ben on 8/23/22, decided to try 

# Rxn_overall_by_Duration %>%
#   filter(!(Intensity %in% c("90"))) %>%
#   filter(ID %in% c("RP 3")) %>%
#   mutate(Genotype = str_extract(Genotype, "Het|WT"),
#          `Stim Length (ms)` = as.factor(`Dur (ms)`)) %>%
#   ggplot(aes(x = Intensity, y = Rxn)) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                fun.min = function(x) mean(x) - se(x),
#                fun.max = function(x) mean(x) + se(x),
#                geom = "errorbar", width = 1, position = position_dodge(0.1)) +
#   stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                geom = "point", position = position_dodge(0.1), size = 3) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
#   labs(title = "Presentation of single or mixed duration stimuli for RP 3",
#        caption = paste("Date:", head(df$Date)),
#        x = "Intensity (dB)",
#        y = "Mean Reaction time (ms)") +
#   scale_x_continuous(breaks = seq(20, 80, by = 10)) +
#   facet_wrap( ~ Duration, ncol = 1) +
#   theme_classic() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
#   )

Rxn_overall_by_Duration %>%
  filter(!(Intensity %in% c("90"))) %>%
  filter(ID %in% c("RP 3")) %>%
  mutate(Genotype = str_extract(Genotype, "Het|WT"),
         `Stim Length (ms)` = as.factor(`Dur (ms)`),
         Phase =  case_when(Phase == "BBN Rotating" ~ "Rotating",
                            TRUE ~ "")) %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
  labs(title = "Presentation of single or mixed duration stimuli for RP 3",
       caption = paste("Date:", head(df$Date)),
       x = "Intensity (dB)",
       y = "Mean Reaction time (ms)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  facet_wrap(Duration ~ Phase, ncol = 2) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )

# Duration in RP 3 --------------------------------------------------------
# 1st rat to complete both the alone and 50-300ms duration allowing for direct comparison.
# After discussion w/ Ben on 8/23/22, decided to try 

# Rxn_overall_by_Duration %>%
#   filter(!(Intensity %in% c("90"))) %>%
#   filter(ID %in% c("RP 3")) %>%
#   mutate(Genotype = str_extract(Genotype, "Het|WT"),
#          `Stim Length (ms)` = as.factor(`Dur (ms)`)) %>%
#   ggplot(aes(x = Intensity, y = Rxn)) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                fun.min = function(x) mean(x) - se(x),
#                fun.max = function(x) mean(x) + se(x),
#                geom = "errorbar", width = 1, position = position_dodge(0.1)) +
#   stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                geom = "point", position = position_dodge(0.1), size = 3) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
#   labs(title = "Presentation of single or mixed duration stimuli for RP 3",
#        caption = paste("Date:", head(df$Date)),
#        x = "Intensity (dB)",
#        y = "Mean Reaction time (ms)") +
#   scale_x_continuous(breaks = seq(20, 80, by = 10)) +
#   facet_wrap( ~ Duration, ncol = 1) +
#   theme_classic() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
#   )

Rxn_overall_by_Duration %>%
  filter(!(Intensity %in% c("90"))) %>%
  filter(ID %in% c("RP 6")) %>%
  mutate(Genotype = str_extract(Genotype, "Het|WT"),
         `Stim Length (ms)` = as.factor(`Dur (ms)`),
         Phase =  case_when(Phase == "BBN Rotating" ~ "Rotating",
                            TRUE ~ "")) %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
  labs(title = "Presentation of single or mixed duration stimuli for RP 6",
       caption = paste("Date:", head(df$Date)),
       x = "Intensity (dB)",
       y = "Mean Reaction time (ms)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  facet_wrap(Duration ~ Phase, ncol = 2) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )

# Duration in all rotating --------------------------------------------------------
# 1st rat to complete both the alone and 50-300ms duration allowing for direct comparison.
# After discussion w/ Ben on 8/23/22, decided to try 

# Rxn_overall_by_Duration %>%
#   filter(!(Intensity %in% c("90"))) %>%
#   filter(ID %in% c("RP 3", "RP 5")) %>%
#   mutate(Genotype = str_extract(Genotype, "Het|WT"),
#          `Stim Length (ms)` = as.factor(`Dur (ms)`),
#          Phase =  case_when(Phase == "BBN Rotating" ~ "Rotating",
#                             TRUE ~ "")) %>%
#   ggplot(aes(x = Intensity, y = Rxn)) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                fun.min = function(x) mean(x) - se(x),
#                fun.max = function(x) mean(x) + se(x),
#                geom = "errorbar", width = 1, position = position_dodge(0.1)) +
#   stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
#                fun = mean,
#                geom = "point", position = position_dodge(0.1), size = 3) +
#   stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
#   labs(title = "Presentation of single or mixed duration stimuli for RP 3 & 5",
#        caption = paste("Date:", head(df$Date)),
#        x = "Intensity (dB)",
#        y = "Mean Reaction time (ms)") +
#   scale_x_continuous(breaks = seq(20, 80, by = 10)) +
#   facet_wrap( ~ Duration, ncol = 1) +
#   theme_classic() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
#   )

Rxn_overall_by_Duration %>%
  filter(!(Intensity %in% c("90"))) %>%
  filter(ID %in% c("RP 5", "RP 3", "RP 6")) %>%
  mutate(Genotype = str_extract(Genotype, "Het|WT"),
         `Stim Length (ms)` = as.factor(`Dur (ms)`),
         Phase =  case_when(Phase == "BBN Rotating" ~ "Rotating",
                            TRUE ~ "")) %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(shape = `Stim Length (ms)`, group = `Stim Length (ms)`),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(linetype = `Stim Length (ms)`, group = `Stim Length (ms)`), fun = mean, geom = "line") +
  labs(title = "Presentation of single or mixed duration stimuli for RP 3 & 5, 6",
       caption = paste("Date:", head(df$Date)),
       x = "Intensity (dB)",
       y = "Mean Reaction time (ms)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  facet_wrap(Duration ~ Phase, ncol = 2) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )

# Duration vs. Genotype ---------------------------------------------------

Rxn_overall_by_Duration %>%
  filter(!(Intensity %in% c("90"))) %>%
  filter(Duration == "Alone") %>%
  mutate(Genotype = str_extract(Genotype, "Het|WT"),
         `Dur (ms)` = as.factor(`Dur (ms)`)) %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  # geom_point(aes(group = ID, color = Genotype), alpha = 0.3)+
  # geom_line(aes(group = ID, color = Genotype), alpha = 0.3)+
  stat_summary(aes(color = Genotype, linetype = `Dur (ms)`, group = interaction(Genotype, `Dur (ms)`)),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(color = Genotype, shape = `Dur (ms)`, group = interaction(Genotype, `Dur (ms)`)),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(color = Genotype, linetype = `Dur (ms)`,, group = interaction(Genotype, `Dur (ms)`)), fun = mean, geom = "line") +
  labs(title = "Tsc2 Eker",
       caption = paste("Date:", head(sort(df$Date, decreasing = TRUE))),
       x = "Intensity (dB)",
       y = "Reaction time (ms, mean +/- SE)") +
  scale_x_continuous(breaks = seq(20, 80, by = 10)) +
  # facet_wrap( ~ `Dur (ms)`, ncol = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255))
  )
