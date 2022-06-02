# Notes -------------------------------------------------------------------

# Package loading ---------------------------------------------------------

# data loading/manipulation
library(readxl); library(tidyverse); library(magrittr); library(dplyr); library(tidyr);

# Analysis
library(psych)

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


# Graph -------------------------------------------------------------------

# Calculate standard error (SE) like standard deviation (SD)
se <- function(x, ...) {sqrt(var(x, ...)/length(x))}


# Overview Graph ----------------------------------------------------------
# Graphs everything for an initial check

Pilot_ABR_data %>%
  # group_by(Rat, Condition, Ear, Freq, dB, Genotype) %>%
  # summarize(RMS = mean(RMS), `W1 Lat` = mean(`W1 Lat`), `W1 Amp` = mean(`W1 Amp`)) %>%
  gather(measure, value, RMS, 'W1 Lat', 'W1 Amp') %>%
  ggplot(aes(x = dB, y = value, color = measure, linetype = Genotype, shape = Genotype, group = interaction(Genotype, measure))) +
    stat_summary(fun = mean,
                 fun.min = function(x) mean(x) - se(x),
                 fun.max = function(x) mean(x) + se(x),
                 geom = "errorbar", width = 1, position = position_dodge(1)) +
    stat_summary(fun = mean, geom = "point", position = position_dodge(1), size = 3) +
    stat_summary(fun = mean, geom = "line") +
    facet_wrap(~ Freq, scale = "free", nrow = 3, strip.position = "top") +
    theme_classic()


