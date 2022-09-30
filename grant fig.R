
# packages ----------------------------------------------------------------

library(tidyverse); library(dplyr); library(tidyr)
library(ggplot2)
library(readxl); library(scales)

se <- function(x, ...) {sqrt(var(x, ...)/length(x))}


# Rxn Table -------------------------------------------------------------------
rbind(Tsc2_rxn, Fmr1_rxn) %>%
  filter(!(Intensity %in% c("90"))) %>%
  filter(Duration == "Alone" & `Dur (ms)` == 50) %>%
  mutate(Genotype = str_extract(Genotype, "Het|WT|KO"),
         Genotype = str_replace(Genotype, "Het", "Tsc2 +/-"),
         Genotype = str_replace(Genotype, "KO", "Fmr1 KO")) %>%
  filter(!(Intensity %in% c(25, 35, 45))) %>%
  # group_by(Genotype, Intensity) %>%
  group_by(ID, Genotype, Intensity) %>%
  # summarise(mean = mean(Rxn), SE = sqrt(var(Rxn)/length(Rxn))) %>%
  summarise(mean = mean(Rxn)) %>%
  write.csv(file = "Rxn.csv")
  

# Rxn Graph ---------------------------------------------------------------
rbind(Tsc2_rxn, Fmr1_rxn) %>%
  filter(!(Intensity %in% c("90"))) %>%
  filter(Duration == "Alone" & `Dur (ms)` == 50) %>%
  mutate(Genotype = str_extract(Genotype, "Het|WT|KO"),
         Genotype = str_replace(Genotype, "Het", "Tsc2 +/-"),
         Genotype = str_replace(Genotype, "KO", "Fmr1 KO")) %>%
  filter(!(Intensity %in% c(25, 35, 45))) %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  # geom_point(aes(group = ID, color = Genotype), alpha = 0.3)+
  # geom_line(aes(group = ID, color = Genotype), alpha = 0.3)+
  stat_summary(aes(color = Genotype, group = Genotype),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", 
               width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(color = Genotype, group = Genotype, shape = Genotype),
               fun = mean, geom = "point", 
               position = position_dodge(0.1), size = 2.5) +
  stat_summary(aes(color = Genotype, group = Genotype), fun = mean, geom = "line") +
  labs(x = "Sound Intensity (dB)",
       y = "Reaction time (ms)") +
  scale_x_continuous(breaks = seq(10, 80, by = 10)) +
  scale_color_manual(values=c("Fmr1 KO" = "Red", "Tsc2 +/-" = "deepskyblue", "WT" = "Black")) +
  # facet_wrap( ~ `Dur (ms)`, ncol = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = rgb(235, 235, 235, 255, maxColorValue = 255)),
    legend.text = element_text(size = "8"),
    legend.title = element_text(size = "8", face = "bold"),
    # legend.spacing.y = unit(1.5, 'mm'),
    legend.key.size = unit(1.5, "mm"),
    legend.position = c(.8,.85),
    legend.background = element_blank(),
    # axis.text=element_text(face="bold"),
    axis.title=element_text(face = "bold")
  )


# Select Graphing Data ----------------------------------------------------
# Can be summarized or not

ABR_3_genotypes <- read_excel("C:/Users/Noelle/Box/Auerbach Lab (Personal)/Tsc ABR Analysis/ABR 3 genotypes.xlsx")

# RMS Table
ABR_3_genotypes  %>%
  mutate(Genotype = str_replace(Genotype, "Het", "Tsc2 +/-"),
         Genotype = str_replace(Genotype, "KO", "Fmr1 KO")) %>%
  filter(!(dB %in% c(15, 25))) %>%
  # spread(Rat, RMS) %>% print
  # group_by(Rat, dB, Genotype) %>%
  # summarise(RMS.avg = mean(RMS), RMS_se = sqrt(var(RMS)/length(RMS)),
  #           Amp = mean(`W1 Amp`), Amp_se = sqrt(var(`W1 Amp`)/length(`W1 Amp`)),
  #           Delay = mean(`W1 Lat`), Delay_se = sqrt(var(`W1 Lat`)/length(`W1 Lat`))) %>%
  # View
  write.csv(file = "ABR.csv")


# RMS Grant Graph ---------------------------------------------------------------
# Signal-to-Noise ratio for BBN, significant for 70-90dB

ABR_3_genotypes  %>%
  mutate(Genotype = str_replace(Genotype, "Het", "Tsc2 +/-"),
         Genotype = str_replace(Genotype, "KO", "Fmr1 KO")) %>%
  ggplot(aes(x = dB, y = RMS, color = Genotype, group = Genotype)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 0) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_continuous(limits = c(20, 90), breaks = seq(20, 90, by = 20)) +
  scale_color_manual(values=c("Fmr1 KO" = "Red", "Tsc2 +/-" = "deepskyblue", "WT" = "Black")) +
  labs(x = "Sound Intensity (dB)",
       y = "Signal-to-Noise Ratio (RMS)") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = "none",
    legend.key.size = unit(1.5, "mm"),
    legend.background = element_blank(),
    legend.text = element_text(size = "8"),
    legend.title = element_text(size = "8", face = "bold"),
    axis.title=element_text(face = "bold")
  )

ABR_3_genotypes  %>%
  mutate(Genotype = str_replace(Genotype, "Het", "Tsc2 +/-"),
         Genotype = str_replace(Genotype, "KO", "Fmr1 KO")) %>%
  ggplot(aes(x = dB, y = `W1 Amp`, color = Genotype, group = Genotype)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 0) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_continuous(limits = c(20, 90), breaks = seq(20, 90, by = 20)) +
  scale_color_manual(values=c("Fmr1 KO" = "Red", "Tsc2 +/-" = "deepskyblue", "WT" = "Black")) +
  labs(x = "Sound Intensity (dB)",
       y = "Wave 1 Amplitued (uV)") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = "none",
    legend.key.size = unit(1.5, "mm"),
    legend.background = element_blank(),
    legend.text = element_text(size = "8"),
    legend.title = element_text(size = "8", face = "bold"),
    axis.title=element_text(face = "bold")
  )


ABR_3_genotypes  %>%
  mutate(Genotype = str_replace(Genotype, "Het", "Tsc2 +/-"),
         Genotype = str_replace(Genotype, "KO", "Fmr1 KO")) %>%
  ggplot(aes(x = dB, y = `W1 Lat`, color = Genotype, group = Genotype)) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 0) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  scale_y_continuous(breaks = seq(0, 2.5, by = 0.2)) +
  scale_x_continuous(limits = c(20, 90), breaks = seq(20, 90, by = 20)) +
  scale_color_manual(values=c("Fmr1 KO" = "Red", "Tsc2 +/-" = "deepskyblue", "WT" = "Black")) +
  labs(x = "Sound Intensity (dB)",
       y = "Wave 1 Delay (ms)") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    panel.grid.major.x = element_line(color = "white"),
    legend.position = c(0.8, 0.8),
    legend.key.size = unit(1.5, "mm"),
    legend.background = element_blank(),
    legend.text = element_text(size = "10"),
    legend.title = element_text(size = "10", face = "bold"),
    axis.title=element_text(face = "bold")
  )
