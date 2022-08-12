# File Breakdown ----------------------------------------------------------
stim = current_file$stim[,,1]

file_settings = stim$para[,,1]

# MATLAB Summary ----------------------------------------------------------
# This is the historic summary that we write down. This should be sanity checked
# against the actual data

results_total_trials = current_file$final.result[,,1]$go.trial.num[1] + current_file$final.result[,,1]$no.go.trial.num[1]
results_hits = current_file$final.result[,,1]$hit.num[1]
results_misses = current_file$final.result[,,1]$miss.num[1]
results_CR = current_file$final.result[,,1]$CR.num[1]
results_FA = current_file$final.result[,,1]$FA.num[1]


# Decode result table -----------------------------------------------------
# Table of reaction times with # of stim

stim_master_list = stim$source.list

# remove sublists that are an artifact of importing
# from: https://stackoverflow.com/questions/15930880/unlist-all-list-elements-in-a-dataframe
stim_master_list = t(apply(stim_master_list, 1, unlist)) %>% as.data.frame()

# add Column names
names(stim_master_list) = append(unlist(stim$stim.tag.list), "Repeat_number", after = 0)

# fix column types as they are all character
stim_master_list = stim_master_list %>% dplyr::mutate_at(vars(Repeat_number, Type, `Freq (kHz)`, `Inten (dB)`, `Dur (ms)`, `Nose Out TL (s)`, `Time Out (s)`),
                                                         ~as.numeric(.))

# Add identifying number (for decoding)
stim_master_list = dplyr::mutate(stim_master_list, "Stim_ID" = row_number())

# Get stim variables automatically
stim_block_size = sum(stim_master_list["Repeat_number"])
stim_type = unique(stim_master_list["Stim Source"]) %>% as.character()

run_data_encoded = data.frame(current_file$result)

# The MATLAB file has 2 extra columns for some unknown reason
if (all(run_data_encoded[7:8] != "0")) {
  stop("What are these columns storing?")
  } else {
  run_data_encoded = run_data_encoded[1:6]
  }

names(run_data_encoded) = list("Time_since_file_start_(s)", "Stim_ID", "Trial_type", "Attempts_to_complete", "Response", "Reaction_(s)")

run_data_encoded = run_data_encoded %>%
                   dplyr::mutate(Response = dplyr::case_when(Response == 1 ~ "Hit",
                                                             Response == 2 ~ "Miss",
                                                             Response == 3 ~ "FA",
                                                             Response == 4 ~ "CR",
                                                             TRUE ~ "ERROR"))

run_data = dplyr::left_join(x = run_data_encoded,
                            y = dplyr::select(stim_master_list, -Repeat_number),
                            by = "Stim_ID", all.x = TRUE)


# File sanity checks ------------------------------------------------------
# Ensure that the MATLAB summary and the calculated summary match

# Calculate the summary statistics
total_trials = run_data %>% dplyr::count() %>% as.numeric()
hits_calc = run_data %>% dplyr::filter(Response == "Hit") %>% dplyr::count() %>% as.numeric()
misses_calc = run_data %>% dplyr::filter(Response == "Miss") %>% dplyr::count() %>% as.numeric()
CRs_calc = run_data %>% dplyr::filter(Response == "CR") %>% dplyr::count() %>% as.numeric()
FAs_calc = run_data %>% dplyr::filter(Response == "FA") %>% dplyr::count() %>% as.numeric()

# Check calculated stats against MATLAB summary stats
if (total_trials != results_total_trials) stop("Trial count miss-match")
if (hits_calc != results_hits) stop("Hit count miss-match")
if (misses_calc != results_misses) stop("Misses count miss-match")
if (CRs_calc != results_CR) stop("Correct Reject (CR) count miss-match")
if (FAs_calc != results_FA) stop("False Alarm (FA) count miss-match")


# Variable cleanup --------------------------------------------------------
# Remove temp variables from the environment as they shouldn't be needed again

# cleanup MATLAB summary
rm(list = c("results_total_trials", "results_hits", "results_misses", "results_CR", "results_FA"))

# cleanup extraneous copies of the current file
rm(list = c("current_file", 'file_settings', 'run_data_encoded', 'stim'))

