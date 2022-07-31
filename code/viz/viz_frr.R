setwd("~/fmv/data/model/FRR/rf/performance")

FRR_perform <- map_dfr(list.files(), read_parquet)


FRR_perform %>%
  mutate(frr_name = fct_reorder(frr_name, nobs)) %>%
  ggplot(aes(nobs, frr_name, fill = frr_name)) +
  geom_bar(stat = "identity") +
  
  scale_fill_manual(values = frr_colors) +
  
  scale_x_continuous(labels = scales::comma,
                     expand = c(0,0),
                     limits = c(0,1.25e+06)) + 
  
  labs(title = "Sample Size by Farm Resource Region",
       subtitle = "Input data for extremely randomized trees, training and testing.",
       y = NULL, x = "Sample Size") +
  
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 20, colour = "grey30",
                            family = "Source Sans Pro"),
        plot.title = element_text(size = 30, face = "bold"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey92"),
        panel.border = element_rect(fill = NA, colour = "grey30"),
        plot.background = element_blank(),
        plot.margin = margin(rep(15,4)))

# R-Sq, MSE, N ####
FRR_perform %>%
  select(frr_name, mse, rsq, nobs) %>%
  mutate(frr_name = fct_reorder(frr_name, rsq)) %>%
  pivot_longer(
    cols = !frr_name,
    names_to = "stat",
    values_to = "value") %>%
  mutate(stat = case_when(
           stat == "rsq" ~ "R-Squared",
           stat == "mse" ~ "Mean Sq. Error",
           stat == "nobs" ~ "Sample Size"
         ),
         stat = fct_relevel(stat, c("R-Squared", "Mean Sq. Error", "Sample Size"))) %>%
  
  ggplot(aes(value, frr_name, fill = stat)) +
  
  geom_bar(stat = "identity") +
  
  facet_wrap(~ stat, scales = "free_x") + 
  
  scale_fill_viridis_d(alpha = 0.8, 
                       begin = 0.2, 
                       end = 0.6) +
  
  scale_x_continuous(labels = scales::comma) + 
  
  labs(title = "Model Performance and Sample Size",
       subtitle = "Farm Resource Region, Extremely Randomized Trees",
       y = NULL, x = NULL) + 
  
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 20, colour = "grey30",
                            family = "Source Sans Pro"),
        plot.title = element_text(size = 30, face = "bold"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey92"),
        panel.border = element_rect(fill = NA, colour = "grey30"),
        strip.background = element_blank(), 
        strip.text = element_text(face = "bold", size = 25),
        plot.background = element_blank(),
        plot.margin = margin(rep(10,4)))
  
  
# FRR Map ####

ag_regions_ref %>%
  mutate(id = as.character(id)) 

frr_colors <- c(`Southern Seaboard` = "#C3AB1B", 
                `Eastern Uplands` = "#71C276", 
                `Basin and Range` = "#EEAB56", 
                `Fruitful Rim` = "#AC842B", 
                `Mississippi Portal` = "#A8D3F2", 
                `Prairie Gateway` = "#A5A5A5", 
                `Northern Great Plains` = "#F3A193", 
                `Northern Crescent` = "#F1EC00", 
                `Heartland` = "#CAC7A1")


usmap::plot_usmap(
  data = ag_regions_ref,
  values = "name",
  regions = c('counties'),
  exclude = c('AK', 'HI'),
  colour = "white",
  size= 0)+
  
  scale_fill_manual(
    values = frr_colors,
    na.value = "#F3A193")+
  
  labs(
    title = "USDA Farm Resource Regions",
    fill = "Farm Resource Region"
  )+
  theme(text = element_text(size = 30, colour = "grey30", family = "Source Sans Pro"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")


