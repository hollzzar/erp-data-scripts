## Source data ##

#Load variables
source("plot_format.R", local=TRUE)

# Load data for each time window and use first row as column names
assign("grand_data", lapply(groups, 
                            function(x) lapply(conditions,
                                               function(y)
                                                 read.table(paste0("erp/", x, "_grand_", y, ".txt"), header=TRUE) %>% 
                                                 mutate(group = x,
                                                        cond = y))))

# Collapse list into a table
grand_data <- bind_rows(grand_data)

# Make data long format for plotting
grand_data <- grand_data %>%
  pivot_longer(cols = -c(group, cond, time), 
               names_to = "channel",
               values_to = "values")

#Set up filler data to use for legend
leg <- grand_data %>%
  dplyr::filter(channel == "Pz" & group == "SUSE")

## Add labels to plots ##

#Get list of plots
plot_list <- plot_labels %>%
  mutate(distribution = tolower(distribution)) %>%
  pull(distribution) %>%
  unique()
# plot_list <- c("cz", "cp1", "cp2", "p3", "p4", "pz", "c3", "c4")
plots <- length(plot_list)

#Make lists of overlays and labels
overlays <- list(p600_overlay, ean_overlay)
x_lab_list <- c(x_mean, x_ean_mean)
plot_titles <- plot_labels %>% 
  pull(distribution) %>% 
  unique()
# plot_titles <- c("Cz", "CP1", "CP2", "P3", "P4", "Pz", "C3", "C4")

#Loop through each plot
for (p in 1:plots) {
  
  #Loop through each group
  for (g in 1:length(groups)) {
    
    #Initialize plot overlay variables
    var <- plot_list[p]
    var_title <- plot_titles[p]
    gr <- groups[g]
    
    #Initialize plot overlay
    name <- paste(var, gr, "overlay", sep = "_")
    assign(name, list())
    
    #Select correct row and columns with labels
    labs <- plot_labels %>%
      dplyr::filter(distribution == var_title &
                      group == gr) %>%
      select(p600_overlay, ean_overlay)
    
    #Get labels and number of columns with significant results
    over <- which(labs != "")
    over_labs <- labs %>% select(c(colnames(labs)[over])) %>% slice() %>% unlist(use.names=FALSE)
    num_over <- length(over)

    #Loop through significant result labels to add overlays to plot
    if (num_over > 0) {
      for (q in 1:num_over) {
        ind <- over[q]
        assign(name, list(get(name), overlays[ind]))
      }
    }

    #Add title and specs to plot
    assign(name, list(get(name), specs, ggtitle(plot_titles[p])))
    
    #Loop through significant result labels to add annotations to plot
    if (num_over > 0) {
      for (r in 1:num_over) {
        ind <- over[r]
        annotes <- list(annotate("text", x=x_lab_list[ind], y=y_lab,
                                 label=over_labs[r], size=lab_size*0.75,
                                 family=font_fam))
        assign(name, list(get(name), annotes))
      }
    }
    
    #Create plot
    plot_name <- paste(var, gr, "plot", sep = "_")
    dat <- grand_data %>% 
      dplyr::filter(channel == var_title &
                      group == gr)
    assign(plot_name,
           ggplot(dat, aes(time, values)))
    
    #Add elements to plot
    assign(plot_name, get(plot_name) + get(name))
    
  }
}

#Buffer plot
buffer_plot <- ggplot(leg, aes(time, values)) +
  stat_summary(fun=mean, geom="line", aes(color=cond), linetype="blank") +
  scale_y_reverse(limits=c(y_min,y_max)) +
  theme_void() +
  ggtitle("") +
  guides(color=FALSE) +
  theme(text=element_text(family=font_fam))

#Legend plot
legend_plot <- buffer_plot +
  overlays +
  annotate("text", x=x_ean_mean, y=y_lab, label="EAN", size=lab_size*0.35, family=font_fam) +
  annotate("text", x=x_mean, y=y_lab, label="P600", size=lab_size*0.35, family=font_fam) +
  leg_specs +
  geom_vline(xintercept=0, linetype="solid") +
  geom_hline(yintercept=0, linetype="solid") +
  guides(shape=FALSE) +
  theme(axis.title=element_text(size=lab_size),
        axis.title.y=element_text(angle=90, margin=margin(r=-1)))

#Theme plot
theme_plot <- buffer_plot +
  scale_color_grey(labels=c("Attested double modal", "Single modal")) +
  guides(color=guide_legend(override.aes=list(color=shade_list,linetype=c("solid","solid")),
                            keywidth=0.25, keyheight=0.25, default.unit="cm")) +
  theme(legend.justification=c(0.5,0), legend.position=c(0.5,0),
        legend.title=element_blank(), legend.text=element_text(size=lab_size*2))

#Create new butterfly plot
if (new_erp == TRUE) {
  library(patchwork)
  (buffer_plot / lf_MAE_plot / lp_MAE_plot / theme_plot) + plot_layout(heights = c(1,2,2,1)) |
    (fz_MAE_plot / cz_MAE_plot / pz_MAE_plot) |
    (buffer_plot / rf_MAE_plot / rp_MAE_plot / legend_plot) + plot_layout(heights = c(1,2,2,1))
  ggsave("MAE_erp.png", dpi=300, width=7, height=5)
  
  (buffer_plot / lf_SUSE_plot / lp_SUSE_plot / theme_plot) + plot_layout(heights = c(1,2,2,1)) |
    (fz_SUSE_plot / cz_SUSE_plot / pz_SUSE_plot) |
    (buffer_plot / rf_SUSE_plot / rp_SUSE_plot / legend_plot) + plot_layout(heights = c(1,2,2,1))
  ggsave("SUSE_erp.png", dpi=300, width=7, height=5)
}

# (cp1_SUSE_plot / cp2_SUSE_plot / p3_SUSE_plot / p4_SUSE_plot) |
#   (c3_SUSE_plot / c4_SUSE_plot / cz_SUSE_plot / pz_SUSE_plot)
# ggsave("SUSE_CP.png", dpi=300)
