





## Training/Testing Density ####

train_density <- usmap::plot_usmap(data = perform_df,
                                   regions = c('counties'),
                                   exclude = c('AK','HI'),
                                   values = "log_n_train",
                                   colour = "grey",
                                   size = 0.3)+
  
  ggplot2::scale_fill_gradientn(
    colours = noltecolors,
    na.value = "grey90",
    limits = c(1,11))+
  
  labs(
    title = "Training Density by County",
    fill = "Obs (log)",
    caption = "N = 3,789,841"
  )+
  theme(
    legend.background = element_blank(),
    legend.position = c(0.9,0),
    text = element_text(size = 20, family = "Source Sans Pro"),
    plot.title = element_text(size = 30, face="bold", hjust = 0.5),
    plot.caption = element_text(size = 20, face = "italic",
                                hjust = 0.5)
  )



test_density <- usmap::plot_usmap(data = perform_df,
                                  regions = c('counties'),
                                  exclude = c('AK','HI'),
                                  values = "log_n_test",
                                  colour = "grey",
                                  size = 0.3)+
  
  ggplot2::scale_fill_gradientn(
    colours = noltecolors,
    na.value = "grey90",
    limits = c(1,11))+
  
  labs(
    title = "Testing Density by County",
    fill = "Obs (log)",
    caption = "N = 1,470,393"
  )+
  theme(
    legend.background = element_blank(),
    legend.position = "none",    
    text = element_text(size = 20, family = "Source Sans Pro"),
    plot.title = element_text(size = 30, face="bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", size = 20, 
                                hjust = 0.5)
  )


library(patchwork)

train_density + test_density
