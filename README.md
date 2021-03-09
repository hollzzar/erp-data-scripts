# ERP data analysis scripts
 
This repository contains the scripts that I used to analyze and plot the ERP data for an experiment on dialectal variation in United States English.

The group analysis files use the processed data from MATLAB to run a series of within- and between-group ANOVAs, format the values, and create data tables that can be passed onto the plotting scripts, output to CSV files, and joined with the cleaned behavioral data for correlation analyses (not included in this repository). The first script, `1a_group_analysis`, is the main analysis script across the whole experiment, while `1b_group_analysis_halves` compares the first and second halves of the experiment.

`2_plot_format` uses the ERP analysis from the main script to pull significant time windows for plotting. It also sets up plotting parameters. `3_erp_plots` automatically generates butterfly plots for the two groups with significant time windows labelled.
