# Select Graphing Data ----------------------------------------------------
# Can be summarized or not

To_Graph = Pilot_ABR_data_summarized %>%
                filter(Type == "BBN")


# RMS Grant Graph ---------------------------------------------------------------
# Signal-to-Noise ratio for BBN, significant for 70-90dB

To_Graph  %>%
  ggplot(aes(x = dB, y = RMS, color = Genotype, group = Genotype)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 4, size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
  scale_color_manual(labels = c("WT" = "Wildtype", "Het" = bquote("Tsc2"^"+/-")),
                     values = c("WT" = "black", "Het" = "#2832C2"),
                     name = NULL) +
  labs(x = "Sound Intensity (dB)",
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

# RMS ANOVA -------------------------------------------------------------------
RMS.aov <- aov(RMS ~ Genotype * dB, data = To_Graph)
summary(RMS.aov)
TukeyHSD(RMS.aov)
TukeyHSD(RMS.aov)$`Genotype:dB` %>% 
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%  
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>% 
  mutate(dB1 = gsub("^.*?WT:(\\d+).*?$","\\1", Comparison),
         dB2 = gsub("^.*?Het:(\\d+).*?$","\\1", Comparison)) %>%
  filter(dB1 == dB2) %>%
  mutate(sig = stars.pval(`p adj`))


# W1 Grant Graph ---------------------------------------------------------------
# Wave 1 Amplitude for BBN to match Fmr1 KO result.

To_Graph  %>%
  # filter(Type == "BBN") %>%
  ggplot(aes(x = dB, y = `W1 Amp`, color = Genotype, group = Genotype)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 3, position = position_dodge(1)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  scale_x_continuous(limits = c(10, 100), breaks = c(20, 40, 60, 80, 100)) +
  scale_color_manual(labels = c("WT" = "Wildtype", "Het" = bquote("Tsc2"^"+/-")),
                     values = c("WT" = "black", "Het" = "#2832C2"),
                     name = NULL) +
  labs(x = "Sound Intensity (dB)",
       y = expression("Wave 1 Amplitued (\u00b5V)")) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = c(0.3, 0.75),
    legend.key = element_blank()
  )

ggsave("R01_W1lat.jpg",
       plot = last_plot(), # or an explicit ggplot object name
       path = ProjectFolder,
       width = 250, height = 250, units = "px", dpi = 100)

# W1 Amp ANOVA -------------------------------------------------------------------
W1amp.aov <- aov(`W1 Amp` ~ Genotype * dB, data = To_Graph)
summary(W1amp.aov)
TukeyHSD(W1amp.aov)$`Genotype:dB` %>% 
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%  
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>% 
  mutate(dB1 = gsub("^.*?WT:(\\d+).*?$","\\1", Comparison),
         dB2 = gsub("^.*?Het:(\\d+).*?$","\\1", Comparison)) %>%
  filter(dB1 == dB2) %>%
  mutate(sig = stars.pval(`p adj`))


# W1 Lat ANOVA ------------------------------------------------------------------
W1lat.aov <- aov(`W1 Lat` ~ Genotype * dB, data = To_Graph)
summary(W1lat.aov)
TukeyHSD(W1lat.aov)$`Genotype:dB` %>% 
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%  
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>% 
  mutate(dB1 = gsub("^.*?WT:(\\d+).*?$","\\1", Comparison),
         dB2 = gsub("^.*?Het:(\\d+).*?$","\\1", Comparison)) %>%
  filter(dB1 == dB2) %>%
  mutate(sig = stars.pval(`p adj`))