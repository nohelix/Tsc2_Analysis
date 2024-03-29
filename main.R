# Notes -------------------------------------------------------------------

# Package loading ---------------------------------------------------------

# data loading/manipulation
library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr);

# Analysis
library(psych); library(gtools);

# Data visualization
library(ggplot2); library(forcats);

# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Global Variables --------------------------------------------------------

ProjectFolder = 'C:/Users/Noelle/Box/Auerbach Lab (Personal)/Tsc ABR Analysis'

# Working directory -------------------------------------------------------
# Set where the data will be loaded from and saved to
setwd(ProjectFolder)

# Import Data -------------------------------------------------------------
# Get the data that has already been copied into an excel sheet

Pilot_ABR_data <- read_excel("C:/Users/Noelle/Box/Auerbach Lab (Personal)/Tsc ABR Analysis/Pilot ABR data.xlsx")


# Data Prep ---------------------------------------------------------------

# Relabel the Frequency to discrete values
Pilot_ABR_data <-
  Pilot_ABR_data %>%
  mutate(Freq = ifelse(Freq == "0", "BBN", paste(Freq, "kHz")),
         Freq = factor(Freq, levels = c("4 kHz", "8 kHz", "16 kHz", "32 kHz", "BBN")),
         Type = ifelse(Freq == "BBN", "BBN", "Tone"),
         Genotype = factor(Genotype),
         dB = factor(dB))


Pilot_ABR_data_summarized <-
  Pilot_ABR_data %>%
  group_by(Rat, Condition, Ear, Freq, dB, Genotype, Type) %>%
  summarize(RMS = mean(RMS), `W1 Lat` = mean(`W1 Lat`), `W1 Amp` = mean(`W1 Amp`))



# RMS ANOVA ---------------------------------------------------------------

RMS.aov <- aov(RMS ~ Genotype * Freq * dB, data = Pilot_ABR_data_summarized)
summary(RMS.aov)
# TukeyHSD(RMS.aov)
TukeyHSD(RMS.aov)$`Genotype:Freq` %>% 
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>% 
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(WT = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         Het = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         p.sig = stars.pval(`p adj`)) %>% 
  filter(WT == Het)

TukeyHSD(RMS.aov)$`Genotype:Freq:dB` %>%
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(WT = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         Het = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         dB1 = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)?:(\\d+).*?$","\\2", Comparison),
         dB2 = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)?:(\\d+).*?$","\\2", Comparison),
         p.sig = stars.pval(`p adj`)) %>%
  filter(WT == Het & dB1 == dB2 & !is.na(`p adj`)) %>% 
  filter(p.sig != " ")


# W1 Amp ANOVA ------------------------------------------------------------

W1amp.aov <- aov(`W1 Amp` ~ Genotype * Freq * dB, data = Pilot_ABR_data_summarized)
summary(W1amp.aov)

TukeyHSD(W1amp.aov)$`Genotype:Freq` %>% 
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>% 
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(WT = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         Het = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         p.sig = stars.pval(`p adj`)) %>% 
  filter(WT == Het)

TukeyHSD(W1amp.aov)$`Genotype:Freq:dB` %>%
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(WT = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         Het = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         dB1 = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)?:(\\d+).*?$","\\2", Comparison),
         dB2 = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)?:(\\d+).*?$","\\2", Comparison),
         p.sig = stars.pval(`p adj`)) %>%
  filter(WT == Het & dB1 == dB2 & !is.na(`p adj`)) %>% 
  filter(p.sig != " ")

# W1 Latency ANOVA --------------------------------------------------------

W1lat.aov <- aov(`W1 Lat` ~ Genotype * Freq * dB, data = Pilot_ABR_data_summarized)
summary(W1lat.aov)

TukeyHSD(W1lat.aov)$`Genotype:Freq` %>% 
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>% 
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(WT = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         Het = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         p.sig = stars.pval(`p adj`)) %>% 
  filter(WT == Het)

TukeyHSD(W1lat.aov)$`Genotype:Freq:dB` %>%
  as_tibble(.name_repair = "unique", rownames = "Comparison") %>%
  filter(grepl("WT:.*?-Het:.*?|Het:.*?-WT:.*?", Comparison)) %>%
  mutate(WT = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         Het = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)-?.*$","\\1", Comparison),
         dB1 = gsub("^.*?WT:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)?:(\\d+).*?$","\\2", Comparison),
         dB2 = gsub("^.*?Het:(4 kHz|8 kHz|16 kHz|32 kHz|BBN)?:(\\d+).*?$","\\2", Comparison),
         p.sig = stars.pval(`p adj`)) %>%
  filter(WT == Het & dB1 == dB2 & !is.na(`p adj`)) %>% 
  filter(p.sig != " ")


# Graph -------------------------------------------------------------------

# Calculate standard error (SE) like standard deviation (SD)
se <- function(x, ...) {sqrt(var(x, ...)/length(x))}

# Select Graphing Data ----------------------------------------------------
# Can be summarized or not
    
To_Graph = Pilot_ABR_data_summarized %>%
              mutate(dB = as.numeric(as.character(dB)))

# Overview Graph ----------------------------------------------------------
# Graphs everything for an initial check

To_Graph %>%
  gather(measure, value, RMS, 'W1 Lat', 'W1 Amp') %>%
  ggplot(aes(x = dB, y = value, color = measure, linetype = Genotype, shape = Genotype, group = interaction(Genotype, measure))) +
    stat_summary(fun = mean,
                 fun.min = function(x) mean(x) - se(x),
                 fun.max = function(x) mean(x) + se(x),
                 geom = "errorbar", width = 1, position = position_dodge(1)) +
    stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
    stat_summary(fun = mean, geom = "line") +
    scale_x_continuous(breaks = c(10,30,50,70,90)) +
    facet_wrap(~ Freq, scales = "free", nrow = 3, strip.position = "top") +
    theme_classic() +
    theme(
      panel.grid.major.x = element_line(color = "grey80")
    )


# RMS Graph ---------------------------------------------------------------
# RMS with color for each Frequency

To_Graph  %>%
  ggplot(aes(x = dB, y = RMS, color = Freq, linetype = Genotype, shape = Genotype, group = interaction(Freq, Genotype))) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_continuous(breaks = c(10,30,50,70,90)) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(color = "grey80")
  )


# Amplitude Graph -----------------------------------------------------------
# Wave 1 Amplitude with color for each Frequency

To_Graph  %>%
  ggplot(aes(x = dB, y = `W1 Amp`, color = Freq, linetype = Genotype, shape = Genotype, group = interaction(Freq, Genotype))) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(1)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
  stat_summary(fun = mean, geom = "line") +
  scale_x_continuous(breaks = c(10,30,50,70,90)) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_line(color = "grey80")
  )


