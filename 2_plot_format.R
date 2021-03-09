## Plot labels ##

#Load ERP data
source("group_analysis.R", local = TRUE)

plot_labels <- win_simple_result %>%
  select(distribution, group, p, window) %>%
  pivot_wider(names_from = window, values_from = p)

#Add labels for each time window
plot_labels <- plot_labels %>%
  mutate(ean_overlay=ifelse(ean <= 0.001, "***",
                            ifelse(ean <= 0.01, "**",
                                   ifelse(ean <= 0.05, "*", ""))),
         p600_overlay=ifelse(p600 <= 0.001, "***",
                           ifelse(p600 <= 0.01, "**",
                                  ifelse(p600 <= 0.05, "*", ""))))

## Legend ##

#Set up dataframe for legend
legend <- tibble(x = c(-200,0,0,200,400,600,800,1000,1200,0,0,0),
                 y = c(0,-4,0,0,0,0,0,0,0,4,2,-2))

#Set up graph variables
shade_list <- c(dm_shade, sm_shade)
psu_grey_light <- paste0(psu_grey,"30")
alpha_lev <- 0.3
y_lab <- -3
y_min <- 5
y_max <- -5
x_min <- 500
x_max <- 900
x_mean <- mean(c(x_min,x_max))
lab_size <- 5
x_ean_min <- 200
x_ean_max <- 400
x_ean_mean <- mean(c(x_ean_min, x_ean_max))
p600_overlay <- list(annotate("rect", xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max, 
                              fill=psu_grey_light, color=psu_grey))
ean_overlay <- list(annotate("rect", xmin=x_ean_min, xmax=x_ean_max, ymin=y_min, ymax=y_max,
                              fill=psu_grey_light, color=psu_grey))

## Plot set-up ##

leg_specs <- list(geom_point(data=legend, aes(x,y,shape=factor(y)), size=lab_size*0.25),
                  geom_text(data=legend[legend$y==0&legend$x!=0,], aes(x,y,label=x), vjust=2.5, size=lab_size*0.275, family=font_fam),
                  geom_text(data=legend[legend$y>0,], aes(x,y,label=y), hjust=3, size=lab_size*0.275, family=font_fam),
                  geom_text(data=legend[legend$y<0,], aes(x,y,label=y), hjust=2.25, size=lab_size*0.275, family=font_fam),
                  scale_shape_manual(values=rep(3,5)),
                  scale_color_manual(values=shade_list),
                  scale_fill_manual(values=shade_list),
                  labs(x="Time (ms)", y=expression(paste("Amplitude (", mu, "V)"))))

specs <- list(leg_specs,
              stat_summary(fun=mean, geom="line", aes(color=cond)),
              geom_vline(xintercept=0, linetype="solid"),
              geom_hline(yintercept=0, linetype="solid"),
              scale_y_reverse(limits=c(y_min,y_max)),
              theme_classic(),
              guides(color=FALSE, shape=FALSE),
              theme(plot.title=element_text(size=lab_size*1.5,hjust=0.17),
                    text=element_text(family=font_fam),
                    axis.ticks = element_blank(), 
                    axis.line = element_blank(), 
                    axis.text = element_blank(),
                    axis.title=element_text(size=lab_size)))
