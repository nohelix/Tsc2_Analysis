# Package loading ---------------------------------------------------------

# data loading/manipulation
library(readxl); library(R.matlab)
source("~/GitHub/Behavior-autoanalysis/fixed import.R")
library(tidyverse); library(dplyr); library(tidyr)

# Analysis
library(psycho); library(stringr)

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
                       '10dB' = 8, '20dB' = 9, '30dB' = 10, '40dB' = 11, '50dB' = 12, '60dB' = 13, '70dB' = 14, '80dB' = 15, '90dB' = 16,
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
  filter(!(Phase %in% c("BBN Training", "Tone Training"))) %>%
  mutate(
    Intensity = gsub("BBN_(.*dB)_(.*?ms)_.*?s", "\\1", File), # extracts intensity from name of file
    Duration = gsub("BBN_(.*dB)_(.*?ms)_.*?s", "\\2", File) # extracts duration from name of file
  )

rm(Tsc_Data_Raw)


# # File Loading Initialization ---------------------------------------------
# # Only run once.
# File_list_temp = head(File_list_possible, 2)
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
  select(Date, File, ID, Phase, Genotype, Intensity, Duration) %>%
  # filter(Date > "2022-01-01") %>%
  # Change date to same format as folder names
  mutate(Folder = as.character.Date(Date) %>% gsub("-", "", .)) #%>% print

# Get Possible files
File_list_possible <-
  File_list %>%
  group_by(Date, File, ID, Phase, Genotype, Intensity, Duration) %>%
  do(
    list.files(path = paste("./data/", .$Folder, sep = ""),
               pattern = paste(gsub(" ", "", .$ID), "_.*.mat", sep = ""),
               # pattern = "\\.mat", # The IDs have a space between color and # but the files don't. This removes the space.
               full.names = TRUE) %>%
      tibble(FileName = .)
  )

# Missing Files
Missing_files <-
left_join(File_list, File_list_possible) %>% 
  filter(is.na(FileName)) %>% 
  filter(Date <= Sys.Date())

View(Missing_files)

# Loaded File list for comparison
File_list_temp = filter(File_list_possible, !(FileName %in% loaded_files))
file_count = nrow(File_list_temp)

writeLines(paste("Loading", nrow(File_list_temp), "files"))


# Load New Files ----------------------------------------------------------
# Note that I have mistaken stim_block for step_size

if(nrow(File_list_temp) > 0) {
  for(i in 1:nrow(File_list_temp)) {       # for-loop over rows
    file = File_list_temp[[i, "FileName"]]
    ID = File_list_temp[[i, "ID"]]
    Date = File_list_temp[[i, "Date"]]
    Phase = File_list_temp[[i, "Phase"]]
    Genotype = File_list_temp[[i, "Genotype"]]
    Intensity = File_list_temp[[i, "Intensity"]]
    Duration = File_list_temp[[i, "Duration"]]
    print(paste("Laoding:", ID, "on", Date))
    current_file = readMat(file)
    source("~/GitHub/Tsc2_Analysis/matlab import.R")
    loaded_files = append(loaded_files, file)
    df = run_data %>%
          mutate(ID = ID, Genotype = Genotype, Date = Date, Stim_Block = stim_block_size, Phase = Phase, Intensity = Intensity, Duration = Duration) %>%
          rbind(df)
    Master_summary = tibble_row(ID = ID, Date = Date, Intensity = Intensity, Duration = Duration, Trials = total_trials, `Hit%` = hits_calc, `FA%` = FAs_calc, Stim_Block_Size = stim_block_size) %>%
                     rbind(Master_summary)
    
    # cleanup extraneous copies of the current file
    rm(list = c("run_data", 'stim_master_list', 'CRs_calc', 'FAs_calc', 'hits_calc', 'misses_calc', 'stim_block_size', 'stim_type', 'total_trials'))
    rm(list = c("Date", "ID", "file", "Phase", "Genotype", "Intensity", "Duration"))
  }
  
  rm(list = c("File_list_temp", "i"))
  writeLines("Done")
} else {writeLines("Done")}


# Threshold Calculation ---------------------------------------------------
# Signal detection index calculation by the psycho package. We use d' a sensitivity measure.
# https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html

# Creates a properly formatted table for psycho by adding the overall CR/FA to each row
dprime_table <- function(df) {
  #print(df)
  check = df %>% filter(Type == 0) %>% count() %>% as.numeric() #%>% print
  CRnum = (if (check == 1) filter(df, Type == 0) %>% .$CR %>% as.numeric() else check) #%>% print
  FAnum = (if (check == 1) filter(df, Type == 0) %>% .$FA %>% as.numeric() else check) #%>% print
  new_df = df %>% filter(Type == 1) %>% 
    mutate(CR = ifelse(is.na(CR), CRnum, CR),
           FA = ifelse(is.na(FA), FAnum, CR),
           Hit = as.numeric(Hit),
           Miss = as.numeric(Miss)) %>% replace(is.na(.), 0) #%>% print
  return(new_df)
}

# Signal detection index calculation
dprime_calc <- function(df) {
  # print(df)
  dprime(n_hit = df$Hit,
         n_fa = df$FA,
         n_miss = df$Miss,
         n_cr = df$CR,
         adjusted = TRUE) %>%
    as_tibble() %>%
    mutate(dB = df$`Inten (dB)`) #%>% print
}

writeLines("Calculating Thresholds")

# Calculate d' and save (along with hit/miss/CR/FA table)
TH_data <-
  df %>%
  group_by(ID, Genotype, Duration, `Dur (ms)`, Type, `Inten (dB)`, Response) %>% 
  summarise(count = n(), .groups = "keep") %>% 
  spread(Response, count) %>% #View
  group_by(ID, Genotype, Duration) %>% #print
  nest() %>%
  mutate(dprime_data = map(data, dprime_table)) %>% 
  select(-data) %>% 
  unnest(cols = c(dprime_data)) %>%
  group_by(ID, Genotype, Duration, `Dur (ms)`) %>% #print
  nest() %>%
  mutate(dprime = map(data, dprime_calc)) %>% #print
  unnest(dprime) #%>% print


# Threshold calculation calculation based on TH_cutoff intercept of fit curve
# LOESS: Local Regression is a non-parametric approach that fits multiple regressions
# see http://r-statistics.co/Loess-Regression-With-R.html
TH_calc <- function(df) {
  # Uncomment to see line fitting by a package which shows line
  # library(drda)
  # drda(dprime ~ dB, data = df) %>% plot
  # print(df)
  fit = loess(dprime ~ dB, data = df)
  # plot(fit)
  TH = approx(x = fit$fitted, y = fit$x, xout = TH_cutoff)$y #%>% print
  return(TH)
}


TH <-
  TH_data %>%
  # filter(Duration == "50-300ms") %>%
  # filter(ID == "RP 6") %>%
  select(ID:`Dur (ms)`, dprime, dB) %>% #print
  group_by(ID, Genotype, Duration, `Dur (ms)`)  %>%
  nest() %>%
  mutate(TH = map_dbl(data, TH_calc)) %>% #print
  select(-data) %>% #print
  mutate(Duration = case_when(Duration == "50ms" ~ "Alone",
                              Duration == "100ms" ~ "Alone",
                              Duration == "300ms" ~ "Alone",
                              Duration == "50-300ms" ~ "50-300 (Mixed)",
                              TRUE ~ as.character(Duration)),
         TH = round(TH, digits = 1))

TH_view <-  
  TH %>%
  spread(`Dur (ms)`, TH)



# Reaction time from summary sheet ----------------------------------------
Rxn <-
  Tsc_Data %>%
  filter(Duration != "50-300ms") %>%
  gather(key = "Intensity", value = "Rxn", 8:16) %>%
  group_by(ID, Genotype, Duration, Intensity) %>%
  summarise(count = n_distinct(Date),
            Rxn = mean(`Rxn`, na.rm = T),
            .groups = "keep")

# Reaction time calculation -----------------------------------------------

writeLines("Calculating average RXN time")

TH_filter <- function(df) {
  # print(df)
  ID = unique(df$ID) # %>% print
  Dur = unique(df$`Dur (ms)`)#  %>% print
  kHz = unique(df$`Freq (kHz)`)
  kHz = if_else(kHz == "0", "BBN", paste0(kHz,"kHz")) # %>% print
  
  # print(TH)
  # print(paste(ID))
  
  cuttoff = TH %>% # have to use UQ to force the evaluation of the variable
    filter(Duration == "Alone") %>% 
    filter(ID == UQ(ID) & `Dur (ms)` == UQ(Dur)) %>% .$TH #%>% print
  
  cuttoff = ifelse(identical(cuttoff, numeric(0)), -99, cuttoff) #%>% print
  # ifelse(identical(cuttoff, numeric(0)), df, filter(df, `Inten (dB)` >= UQ(cuttoff))) %>% print
  
  df %>%
    filter(`Inten (dB)` >= UQ(cuttoff)) # %>% print
}

Data_over_TH <-
  df %>%
  filter(Type == 1 & Response == "Hit") %>%
  filter(`Inten (dB)` != -100) %>%
  mutate(Rat = .$ID, Dur = .$`Dur (ms)`) %>%
  group_by(Rat, Genotype, Dur, Duration) %>%
  nest %>%
  mutate(data = map(data, TH_filter)) #%>% print


Rxn_overall <-
  Data_over_TH %>%
  ungroup() %>% 
  unnest(data) %>% 
  filter(Type == 1 & Response == "Hit") %>%
  filter(`Inten (dB)` != -100) %>% 
  # Filter by TH table so that only reaction times above thresholds are included
  group_by(ID, Genotype, `Dur (ms)`, `Inten (dB)`) %>%
  summarise(count = n_distinct(Date),
            Rxn = mean(`Reaction_(s)`, na.rm = TRUE) * 1000, 
            .groups = "drop") %>%
  rename(Intensity = `Inten (dB)`)


Rxn_overall_by_Duration <-
  Data_over_TH %>%
  ungroup() %>% 
  unnest(data) %>% 
  filter(Type == 1 & Response == "Hit") %>%
  filter(`Inten (dB)` != -100) %>% 
  # Filter by TH table so that only reaction times above thresholds are included
  group_by(ID, Genotype, Duration, `Dur (ms)`, `Inten (dB)`) %>%
  summarise(count = n_distinct(Date),
            Rxn = mean(`Reaction_(s)`, na.rm = TRUE) * 1000, 
            .groups = "drop") %>%
  rename(Intensity = `Inten (dB)`) %>%
  mutate(Duration = case_when(Duration == "50ms" ~ "Alone",
                              Duration == "100ms" ~ "Alone",
                              Duration == "300ms" ~ "Alone",
                              Duration == "50-300ms" ~ "Mix"))



# Graphing ----------------------------------------------------------------
# Calculate standard error (SE) like standard deviation (SD)
se <- function(x, ...) {sqrt(var(x, ...)/length(x))}



# # Clean an entry ----------------------------------------------------------
# 
# # Find string in loaded list
# str_which(loaded_files, "20220817.*RP5")
# # Check for correct entry
# loaded_files[[221]]
# # remove item from list
# loaded_files = loaded_files[-221]
# 
# # Check lines you are deleting
# df %>% filter((Date < "2022-08-17" & Date > "2022-08-16" & ID == "RP 5"))
# # Clean files from the master dataframe
# df = df %>% filter(!(Date < "2022-08-17" & Date > "2022-08-16" & ID == "RP 5"))
  