# Package loading ---------------------------------------------------------

# data loading/manipulation
library(R.matlab);
source("~/GitHub/Behavior-autoanalysis/fixed import.R")
library(tidyverse); library(dplyr); library(tidyr)

# Analysis

# Global Variables --------------------------------------------------------

# The folder where the project subfolders are:
MainFolder = 'C:/Users/Noelle/Box/Behavior Lab/Projects (Behavior)/Tsc2 Eker'

# Minimum number of blocks of trials to keep (vs. discard day) not counting the
# 1st block which will be dropped
min_blocks = 5

# Sensitivity cutoff for determining hearing thresholds
TH_cutoff = 1.5


# Working directory -------------------------------------------------------
setwd(MainFolder)

# Xlsx multisheet import --------------------------------------------------
# Read All Excel Sheets
# Modified from https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
read_excel_allsheets <- function(filename, range, sheetlist, tibble = FALSE) {
  # but if you like tidyverse tibbles (the default with read_excel) then just pass tibble = TRUE
  #sheets <- readxl::excel_sheets(filename)
  #sheetname <- lapply(sheets, function(X) read_excel(filename, sheet = X, range = "A1:A1", col_names = FALSE) %>% toString %>% print)
  
  #iterate through each sheet (lapply) and create an individual dataframe
  x <- lapply(sheetlist, function(X) readxl::read_excel(filename, sheet = X, range = range) %>%
                # hard-coded column selection; this will fail if the columns get moved - i.e. Fmr1 Group 1
                select(1:4, 6:8, 11:20, 24) %>%
                mutate_at(c(8:16), as.numeric) %>%
                rename(Date = 1, File = 2,
                       Weight = 3, WeightChange = 4,
                       Trials = 5, Hit = 6, FA = 7,
                       '10db' = 8, '20db' = 9, '30dB' = 10, '40dB' = 11, '50dB' = 12, '60dB' = 13, '70dB' = 14, '80dB' = 15, '90dB' = 16,
                       TH = 17, Phase = 18) %>%
                # remove empty days
                filter(!(is.na(Date))) %>%
                filter(File != "Did not run") %>%
                # change type of columns
                mutate(Trials = as.numeric(Trials),
                       Hit = as.numeric(Hit),
                       FA = as.numeric(FA)) %>%
                # Add extra columns to deal with the eventual concatenation (typical wide to tall conversion)
                mutate(ID = X,
                       Genotype = read_excel(filename, sheet = X, range = "A2:A2", col_names = FALSE) %>% toString())
              )
  
  # Converts from the default tibble structure (flexible, easy to read, less predictable) to hard data frame
  if (!tibble) x <- lapply(x, as.data.frame)
  
  # name the elements in the list with the sheet names
  names(x) <- gsub("\\s", "", sheetlist)
  
  # returns list
  x
}
# Import Data -------------------------------------------------------------
# Note that the excel spreadsheet range is hardcoded to 9,999 lines, so if the
# data exceds that, it will not be loaded.

# Load Tsc spreadsheet (hard coded)
RatID_list <- excel_sheets("Tsc2_Eker_Gp1_RP_GP_TP.xlsx")

# The message creation for the log is slow & spammy so I have suppressed the 'New Name' message
Tsc_Data_Raw <- read_excel_allsheets("Tsc2_Eker_Gp1_RP_GP_TP.xlsx", 
                                     range = "A3:X600", sheetlist = RatID_list, tibble = TRUE)

# Data Processing ---------------------------------------------------------

Tsc_Data <-
  Tsc_Data_Raw %>%
  # Concat tables
  bind_rows() %>%
  # remove empty days
  filter(!(is.na(File))) %>%
  # Filter unclassified days
  filter(!(is.na(Phase))) %>%
  # Filter out FA correction, Maintenance, & Errors
  filter(!(Phase %in% c("FA correction", "Maintaince", "Error"))) %>%
  # Filter out Training and retraining
  filter(!(Phase %in% c("BBN Training"))) %>%
  mutate(
    Intensity = gsub("BBN_(.*dB)_(.*?ms)_.*?s", "\\1", File), # extracts intensity from name of file
    Duration = gsub("BBN_(.*dB)_(.*?ms)_.*?s", "\\2", File) # extracts duration from name of file
  )

rm(Tsc_Data_Raw)


# # File Loading Initialization ---------------------------------------------
# # Only run once.
# # File_list_temp = head(File_list_possible, 2)
# df = tibble()
# Master_summary = tibble()
# loaded_files = list()

# Get & verify raw files list ---------------------------------------------
# create a list of all relevant files (daily runs) that need to be imported
# and checks filenames on filesystem vs summary xls log's expected filenames
# popup for outright missing files (empty popup = no problems)
# warnings of mismatches export to xls

# This needs to walk a directory, find the .csv file that starts with the same
# name START as the 'file' column.

# Get folder and file names
File_list <-
  Tsc_Data %>%
  # Select date and file columns
  select(Date, File, ID, Phase) %>%
  # filter(Date > "2022-01-01") %>%
  # Change date to same format as folder names
  mutate(Folder = as.character.Date(Date) %>% gsub("-", "", .)) #%>% print

# Get Possible files
File_list_possible <-
  File_list %>%
  group_by(ID, Date, File, Phase) %>%
  do(
    list.files(path = paste("./data/", .$Folder, sep = ""),
               pattern = paste(gsub(" ", "", .$ID), "_.*.mat", sep = ""),
               # pattern = "\\.mat", # The IDs have a space between color and # but the files don't. This removes the space.
               full.names = TRUE) %>%
      tibble(FileName = .)
  )

# Loaded File list for comparison
File_list_temp = filter(File_list_possible, !(FileName %in% loaded_files))


# Load New Files ----------------------------------------------------------
for(i in 1:nrow(File_list_temp)) {       # for-loop over rows
  file = File_list_temp[[i, "FileName"]]
  ID = File_list_temp[[i, "ID"]]
  Date = File_list_temp[[i, "Date"]]
  print(paste("Laoding:", ID, "on", Date))
  current_file = readMat(file)
  source("~/GitHub/Tsc2_Analysis/matlab import.R")
  loaded_files = append(loaded_files, file)
  df = run_data %>%
        mutate(ID = ID, Date = Date) %>%
        rbind(df)
  Master_summary = tibble_row(ID = ID, Date = Date, Trials = total_trials, `Hit%` = hits_calc, `FA%` = FAs_calc, Stim_Block = stim_block_size) %>%
                   rbind(Master_summary)
  
  # cleanup extraneous copies of the current file
  rm(list = c("run_data", 'stim_master_list', 'CRs_calc', 'FAs_calc', 'hits_calc', 'misses_calc', 'stim_block_size', 'stim_type', 'total_trials'))
  rm(list = c("Date", "ID", "file"))
}

rm(list = c("File_list_temp", "i"))


# Daily summary from raw data -------------------------------------------------------------
# Calculates hit rate, false alarm rate, and trial count from data across conditions
# Summarized by day and individual

writeLines("Analyzing summary data (trials/hits/FAs)")

# Summarize Hits, misses, FAs, & CRs for each individual by day
Hit_summary <-
  Tsc_Data %>%
  # Summarize for each individual by day
  group_by(ID, Genotype, Duration, Phase) %>%
  summarise(count = n_distinct(Date),
            Trials = mean(Trials, na.rm = T),
            Hit = mean(`Hit`, na.rm = T),
            FA = mean(`FA`, na.rm = T),
            .groups = "keep")


# Reaction time calculation -----------------------------------------------

writeLines("Calculating average RXN time")

Rxn <-
  Tsc_Data %>%
  filter(Duration != "50-300ms") %>%
  gather(key = "Intensity", value = "Rxn", 8:16) %>%
  group_by(ID, Genotype, Duration, Intensity) %>%
  summarise(count = n_distinct(Date),
            Rxn = mean(`Rxn`, na.rm = T),
            .groups = "keep")

  # ANOVA
  Rxn.aov = aov(Rxn ~ Intensity * Duration * Genotype, data = Rxn)
  
  # Parametric check
  Rxn.aov$residuals %>%
    shapiro.test()
  
  # # Summary
  # summary(Rxn.aov)
  
  # Non-Parametric ANOVA
  kruskal.test(Rxn ~ interaction(Duration,Genotype, Intensity), data = Rxn)
  
# Rxn Plot ----------------------------------------------------------------
# Plot by animal pre & post & group
  
# Calculate standard error (SE) like standard deviation (SD)
se <- function(x, ...) {sqrt(var(x, ...)/length(x))}


Rxn %>%
  filter(!(Intensity %in% c("10db", "20db", "90dB"))) %>%
  filter(Duration != "50-300ms") %>%
  ggplot(aes(x = Intensity, y = Rxn)) +
  # geom_line(aes(color = Condition, linetype = ID))+
  # geom_point(aes(color = Condition, fill = ID))+
  # geom_point(aes(group = ID), color = "grey70")+
  # geom_line(aes(group = ID, color = Condition))+
  stat_summary(aes(color = Genotype, group = Genotype),
               fun = mean,
               fun.min = function(x) mean(x) - se(x),
               fun.max = function(x) mean(x) + se(x),
               geom = "errorbar", width = 1, position = position_dodge(0.1)) +
  stat_summary(aes(color = Genotype, group = Genotype),
               fun = mean,
               geom = "point", position = position_dodge(0.1), size = 3) +
  stat_summary(aes(color = Genotype, group = Genotype), fun = mean, geom = "line") +
  labs(title = "Tsc Eker",
       x = "Intensity (dB)",
       y = "Reaction time (ms, mean +/- SE)") +
  facet_wrap(~ fct_relevel(Duration, "300ms", "100ms", "50ms"), ncol = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_line(color = "white")
    # panel.grid.minor.x = element_line(color = "grey80"))
  )

  