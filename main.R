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


