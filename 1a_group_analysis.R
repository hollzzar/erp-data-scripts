## Data wrangling ##

library(afex)

# Load behavioral data
source("behavioral_data.R", local=TRUE)

# Create variables
windows <- c("ean", "p600")
conditions <- c("DM", "SM")
analyses <- c("center", "butterfly")
groups <- unique(summary_back$dialect)
channels <- c("Fz", "Cz", "Pz", "LF", "RF", "LP", "RP")
table_cols <- c("iv", "df_num", "df_den", "MSE", "F", "eta_sq", 
                "p", "window", "distribution")

# Set up ANOVA parameters
set_sum_contrasts()

# Load data for each time window and use first row as column names
assign("erp_data", lapply(windows, function(x) 
  read.table(paste0("erp/", "all_", x, ".txt"), header=TRUE) %>% 
    mutate(window = x)))

# Collapse list into a table
erp_data <- bind_rows(erp_data)

## Main ERP data ##

# Get dialect assignment for each participant
erp_data <- summary_back %>% 
  select(ID, dialect) %>% 
  mutate(ID = as.integer(ID)) %>%
  right_join(erp_data, by = c("ID" = "ERPset"))

# Reshape data
erp_data <- erp_data %>%
  mutate(cond = ifelse(bini %in% c("3", "4", "5"), "SM",
                       ifelse(bini %in% c("6", "7", "8"), "DM", bini)),
         cond = factor(cond, levels = conditions, labels = conditions),
         group = factor(dialect, levels = groups, labels = groups),
         distribution = ifelse(chlabel %in% c("Fz","Cz","Pz"), "center", 
                               ifelse(chlabel %in% c("LF","RF","LP","RP"), 
                                      "butterfly", "other")))

# write.csv(erp_data, "erp_data.csv")

## Within-group analysis ##

# Run ANOVAs in each time window (2) and distribution (2)
win_table <- lapply(windows, 
                    function(x) lapply(analyses, 
                                       function(y) lapply(groups, 
                                                          function(z)
                                                            aov_ez(id="ID", dv="value", 
                                                                   erp_data %>% dplyr::filter(window == x & 
                                                                                                distribution == y & 
                                                                                                group == z),
                                                                   within=c("chlabel", "cond")))))

# Pull ANOVA tables
win_result <- lapply(1:length(windows), 
                     function(x) lapply(1:length(analyses), 
                                        function(y) lapply(1:length(groups), 
                                                           function(z)
                                                             win_table[[x]][[y]][[z]]$anova_table %>% 
                                                             data.frame() %>% 
                                                             rownames_to_column() %>%
                                                             mutate(window = windows[x],
                                                                    distribution = analyses[y],
                                                                    group = groups[z]))))

# Collapse tables into one dataframe
win_result <- bind_rows(lapply(win_result, function(x) 
  bind_rows(lapply(x, function(y) bind_rows(y)))))

# Rename columns
colnames(win_result) <- c(table_cols, "group")

## Within-group simple effects ##

# Run ANOVAs in each time window (2) and channel (7)
win_simple_table <- lapply(windows, 
                           function(x) lapply(channels, 
                                              function(y) lapply(groups, 
                                                                 function(z)
                                                                   aov_ez(id="ID", dv="value", 
                                                                          erp_data %>% dplyr::filter(window == x & chlabel == y & group == z),
                                                                          within=c("cond")))))
# Pull ANOVA tables
win_simple_result <- lapply(1:length(windows), 
                            function(x) lapply(1:length(channels), 
                                               function(y) lapply(1:length(groups), 
                                                                  function(z)
                                                                    win_simple_table[[x]][[y]][[z]]$anova_table %>%
                                                                    data.frame() %>% 
                                                                    rownames_to_column() %>%
                                                                    mutate(window = windows[x],
                                                                           chlabel = channels[y],
                                                                           group = groups[z]))))

# Collapse tables into one dataframe
win_simple_result <- bind_rows(lapply(win_simple_result, function(x) 
  bind_rows(lapply(x, function(y) bind_rows(y)))))

# Rename columns
colnames(win_simple_result) <- c(table_cols, "group")

## Between-group analysis ##

# Run ANOVAs in each time window (2) and distribution (2)
btx_table <- lapply(windows, function(x) lapply(analyses, function(y) 
  aov_ez(id="ID", dv="value", 
         erp_data %>% dplyr::filter(window == x & distribution == y),
         within=c("chlabel", "cond"), between=c("group"))))

# Pull ANOVA tables
btx_result <- lapply(1:length(windows), 
                     function(x) lapply(1:length(analyses), 
                                        function(y)
                                          btx_table[[x]][[y]]$anova_table %>% 
                                          data.frame() %>% 
                                          rownames_to_column() %>%
                                          mutate(window = windows[x],
                                                 distribution = analyses[y])))

# Collapse tables into one dataframe
btx_result <- bind_rows(lapply(btx_result, function(x) bind_rows(x)))

# Rename columns
colnames(btx_result) <- table_cols

# Remove intermediate variables
# rm(btx_table, win_simple_table, win_table)

## Clean up final tables ##

# Within-group analysis: overall
win_result <- win_result %>%
  dplyr::filter(str_detect(iv, "cond") == TRUE) %>%
  mutate(df_num = df_formatting(df_num),
         df_den = df_formatting(df_den),
         MSE = round(MSE, 2),
         `F` = t_formatting(`F`),
         eta_sq = round(eta_sq, 2),
         p_str = p_formatting(p, 1)) %>% 
  rowwise() %>% 
  mutate(apa = apa_formatting(c(df_num, df_den), `F`, p_str))

# Within-group analysis: simple effects
win_simple_result <- win_simple_result %>%
  dplyr::filter(str_detect(iv, "cond") == TRUE) %>%
  mutate(df_num = df_formatting(df_num),
         df_den = df_formatting(df_den),
         MSE = round(MSE, 2),
         `F` = t_formatting(`F`),
         eta_sq = round(eta_sq, 2),
         p_str = p_formatting(p, 1)) %>% 
  rowwise() %>% 
  mutate(apa = apa_formatting(c(df_num, df_den), `F`, p_str))

# Combine within-group tables
win_result_comb <- union(win_result, win_simple_result)

# Between-group analysis: overall
## Interactions with group n.s.
btx_result <- btx_result %>%
  mutate(df_num = df_formatting(df_num),
         df_den = df_formatting(df_den),
         MSE = round(MSE, 2),
         `F` = t_formatting(`F`),
         eta_sq = round(eta_sq, 2),
         p_str = p_formatting(p, 1)) %>% 
  rowwise() %>% 
  mutate(apa = apa_formatting(c(df_num, df_den), `F`, p_str))

## Additional CP ROI data ##

# Load additional dataset with CP ROI electrodes
roi_data <- read.table("erp/all_cp_roi.txt", header=TRUE)

# Get dialect assignment for each participant
roi_data <- summary_back %>% 
  select(ID, dialect) %>% 
  mutate(ID = as.integer(ID)) %>%
  right_join(roi_data, by = c("ID" = "ERPset"))

# Reshape data
roi_data <- roi_data %>%
  mutate(cond = ifelse(bini %in% c("3", "4", "5"), "SM",
                       ifelse(bini %in% c("6", "7", "8"), "DM", bini)),
         cond = factor(cond, levels = conditions, labels = conditions),
         group = factor(dialect, levels = groups, labels = groups))

# Create smaller CP ROI
small_roi <- roi_data %>%
  dplyr::filter(chlabel %in% c("Cz", "CP1", "CP2", "P3", "P4", "Pz")) %>%
  group_by(ID, cond) %>%
  summarise(small_CP = mean(value), .groups = "keep") %>%
  pivot_wider(names_from = "cond", values_from = "small_CP") %>%
  mutate(small_CP = abs(DM - SM)) %>%
  select(ID, small_CP) %>%
  mutate(ID = as.character(ID))

# write.csv(roi_data, "roi_data.csv")

## Correlation analysis ##

# Pull mean values from erp_data
amp_measure <- erp_data %>%
  select(ID, value, window, cond, chlabel) %>%
  dplyr::filter((chlabel == "LF" & window == "ean") | 
                  (chlabel == "CP" & window == "p600")) %>%
  pivot_wider(names_from = cond, values_from = value) %>%
  mutate(amp = abs(DM - SM)) %>%
  select(ID, window, amp) %>%
  pivot_wider(names_from = window, values_from = amp) %>%
  mutate(ID = as.character(ID))

# Pull mean values from small_roi
amp_measure <- amp_measure %>%
  left_join(small_roi, by = "ID")

# Add to summary dataset
summary_back <- summary_back %>%
  left_join(amp_measure, by = "ID")

# Remove variables
rm(btx_table, win_table, win_simple_table, table_cols, amp_measure)

## Output files ##

# write.csv(btx_result, "btx_group_erp.csv")
# write.csv(win_result_comb, "win_group_erp.csv")
# write.csv(summary_back, "behav_data.csv")