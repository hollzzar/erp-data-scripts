# Set behavioral variables for if loops
code_geo <- FALSE
SUSE_tab <- FALSE
new_image <- FALSE

# Load behavioral data
source("DM_attitudes_survey.R", local=TRUE)

# Mark participants with/without behavioral or ERP data
summary_back <- summary_back %>%
  mutate(erp_code = ifelse(ID %in% all_erp, 1, 0),
         behav_code = ifelse(is.na(double_acceptable), 0, 1))