# ERP data analysis scripts
 
This repository contains the scripts that I used to analyze and plot the ERP data for an experiment on dialectal variation in United States English.

There are two relevant background files for the ERP analyses. `0a_global_params` loads packages, pulls participant numbers from MATLAB for exclusion/inclusion, establishes global formatting parameters for plotting, and creates helper functions for number formatting (e.g., *p* values). These helper functions were mainly used in writing the manuscript. `0b_behavioral` is the top-level script that references several underlying data-wrangling scripts for the various surveys and other pieces of background information (not included in this repository). Its primary output is `summary_back`, which as the name implies is a summary of all the background/behavioral data for each participant.

The group analysis files use the processed data from MATLAB to run a series of within- and between-group ANOVAs, format the values, and create data tables that can be passed onto the plotting scripts, output to CSV files, and joined with the cleaned behavioral data for correlation analyses (not included in this repository). The first script, `1a_group_analysis`, is the main analysis script across the whole experiment, while `1b_group_analysis_halves` compares the first and second halves of the experiment.

`2_plot_format` uses the ERP analysis from the main script to pull significant time windows for plotting. It also sets up plotting parameters. `3_erp_plots` automatically generates butterfly plots for the two groups with significant time windows labelled.
