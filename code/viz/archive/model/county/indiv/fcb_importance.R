
# VIZ
ggplot(data = fcb_imp,
       aes(Importance, 
           reorder(group, imp_med))) +
  
  stat_boxplot(geom = "errorbar",
               size = 0.5) +
  geom_boxplot(size = 0.4, 
               colour = "black", outlier.size = 1,
               outlier.alpha = 0.1, outlier.colour = "black") +
  
  geom_vline(
    xintercept = 0
  )+
  
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(0, 3, by = 0.5)) +
  scale_y_discrete(expand = c(0,0)) +
  
  labs(
    x = "Feature Importance",
    y = NULL)+
  
  theme(
    text = element_text(family = "sans", size = 13),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 17),
    axis.text.y = ggtext::element_markdown(size = 10),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, colour = "grey70"),
    panel.grid.major.y = element_blank()
    )

