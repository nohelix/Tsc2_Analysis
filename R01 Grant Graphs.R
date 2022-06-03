# Select Graphing Data ----------------------------------------------------
# Can be summarized or not

To_Graph = Pilot_ABR_data_summarized


# RMS Grant Graph ---------------------------------------------------------------
# Signal-to-Noise ratio for BBN

To_Graph  %>%
  filter(Type == "BBN") %>%
  ggplot(aes(x = dB, y = RMS, color = Genotype, group = Genotype)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 4, size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
  scale_color_manual(labels = c("WT" = "Wildtype", "Het" = bquote("Tsc2"^"+/-")),
                     values = c("WT" = "black", "Het" = "blue"),
                     name = NULL) +
  labs(x = "Sound Intensity",
       y = "Signal-to-Noise Ratio (RMS)") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = c(0.7, 0.25)
  )

ggsave("R01_RMS.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 250, height = 250, units = "px", dpi = 100)

# W1 Grant Graph ---------------------------------------------------------------
# Wave 1 Amplitude for all sounds b/c BBN not great.

To_Graph  %>%
  ggplot(aes(x = dB, y = `W1 Amp`, color = Genotype, group = Genotype)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 3, position = position_dodge(1)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
  scale_color_manual(labels = c("WT" = "Wildtype", "Het" = bquote("Tsc2"^"+/-")),
                     values = c("WT" = "black", "Het" = "blue"),
                     name = NULL) +
  labs(x = "Sound Intensity",
       y = "Wave 1 Amplitued") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = c(0.35, 0.75)
  )

ggsave("R01_W1lat.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 250, height = 250, units = "px", dpi = 100)
