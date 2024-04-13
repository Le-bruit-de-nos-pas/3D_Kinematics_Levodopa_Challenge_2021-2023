
library(tidyverse)
library(data.table)
options(scipen = 999)

output_wide <- fread("Source/output_wide_BestON_OFF.csv")
names(output_wide)
unique(output_wide$Group)
dim(output_wide)


palette <- c( "#D45769", "#b1d7ec", "#0099E0","#0072BB", "#00468B")
# palette <- c( "#D45769", "#00468B")
palette <- c(  "#b1d7ec", "#D45769","#00468B")

range(output_wide$wrist_dev_mean_vel_worstside)


output_wide  %>%
  mutate(Group=factor(Group, levels=c("OFF", "BESTON", "Control"))) %>%
  ggplot(aes(x = as.factor(Group), y = as.numeric(wrist_dev_mean_vel_worstside), 
                                     fill=as.factor(Group), colour=as.factor(Group) )) +
  theme_minimal() +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.7, adjust = 0.8) +
  geom_point(position = position_jitter(width=0.25, height = 0.0052), size=3, stroke =3, shape=1, alpha = 1) +
  labs(x = "", y = "Score \n", title="Wrist Deviation Vel. \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15, vjust = -0.5),
        axis.title.y = element_text(size = 15, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 85, by = 20)) +
  coord_cartesian(ylim = c(-0.01, 85))

