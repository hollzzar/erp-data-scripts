# Package list
pkg_list <- c("plyr","tidyverse", "data.table", "ggplot2", "maps", "kableExtra", "cowplot", "officer",
              "Hmisc", "weights", "scales", "ggmap", "extrafont", "corrplot", "RColorBrewer", "stringr")

# Load packages
pacman::p_load(pkg_list, character.only = TRUE)

# Set ID filters
unusable <- c("0","00","107","202","122")
erp_reject <- read.table('~/Mirror/DM_analysis_scripts/r_scripts/erp/ERP_reject.txt') %>% pull(V1)

# Get participant lists from MATLAB
all_erp <- read_table('~/Mirror/DM_analysis_scripts/r_scripts/erp/ERP_keep.txt', 
                      col_names='ID', col_types=cols(ID=col_double())) %>%
  pull(ID) %>%
  as.character()

# Set global color scheme
psu_grey <- "#d7d8db"
sm_shade <- "#a6bddb"
sm_shade_2 <- "#d0d1e6"
sm_shade_1 <- "#f6eff7"
dm_shade_1 <- "#67a9cf"
dm_shade_2 <- "#1c9099"
dm_shade <- "#016c59"

# Set global font
font_fam <- "LM Roman 10"
text_size <- 12

# Acceptability and intelligibility judgment scale mean
scale_mean <- mean(c(1,5))

# Familiarity scale mean
scale_mean_fam <- mean(c(1,4))

# Make logical operator that looks for a value that's not in a column
"%notin%" <- Negate("%in%")

# Make number formatting function for p values
p_formatting <- function(val, format_code = 1) {
  
  if (format_code == 1) {
    
    sign_type <- if_else(val < 0.001, 
                         "<", 
                         "=")
    
    val <- if_else(sign_type == "<", 
                   ".001",
                   substring(sprintf("%.3f", val), 2))
    
    val_string <- paste(sign_type, val, sep = " ")
    val_string
    
  } else if (format_code == 0) {
    
    val <- sprintf("%.3f", val)
    val_string <- substring(val, 2)
    val_string
    
  }
  
}

# Make number formatting function for t values
t_formatting <- function(val) {
  val <- abs(val)
  val <- sprintf("%.2f", val)
  val
}

# Make number formatting function for reaction times
rt_formatting <- function(val) {
  val <- sprintf("%.0f", val)
  val
}

# Make number formatting function for percentages
pct_formatting <- function(val) {
  val <- val %>%
    prod(100) %>%
    sprintf("%.2f", .) %>%
    paste0("%")
  val
}

# Make number formatting function for degrees of freedom
df_formatting <- function(val) {
  
  val <- if_else(val%%1 == 0, sprintf("%.0f", val), sprintf("%.2f", val))
  
  val
}

# Make function to output APA formatting
apa_formatting <- function(df, f_val, p_val) {
  
  df <- c(df)
  
  if (length(df) == 2) {
    sprintf("*F*(%s, %s) = %s, *p* %s", df[1], df[2], f_val, p_val)
  } else {
    sprintf("*t*(%s) = %s, *p* %s", df, f_val, p_val)
  }
  
}

# Make number formatting function for correlation values
corr_formatting <- function(corr_val) {
  
  val <- sprintf("%.2f", corr_val)
  val_string <- substring(val, 2)
  val_string
  
}
