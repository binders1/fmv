# Load package ####


# Load FRR importance (ffb) ####
ffb_imp <-
  loadResults("ffb", "importance")

## Load Nolte (2020) features
root <- "~/fmv"
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
noltevars_path <- file.path(ddir, "nolte2020vars.csv")

nolte2020vars <- 
  read_csv(noltevars_path,
           show_col_types = F) %>% 
  pull() %>%
  c(., "ha", "x45", "y45")

# Source Prep ####
source(file.path(cdir, "functions/sourceFuncs.R"))
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(cdir, "viz/model/frr/viz_frr_prep.R"))


# Load font ####
font <- "Open Sans"
loadFont(font)


# Clean FRR Importance data ####

ffb_imp_clean <-
  
  ffb_imp %>%
  
  # merge with frr names 
  left_join(
    ag_regions_key
  ) %>%
  relocate(frr = "frr_name") %>%
  
  select(!c(type, id)) %>%
  
  pivot_longer(
    cols = !c(frr, model),
    names_to = "Variable",
    values_to = "Importance") %>%
  
  # group climatic variables
  mutate(group = case_when(
    str_detect(Variable, "^Dew") ~ "DewTemp",
    str_detect(Variable, "(?<!Dew)Temp") ~ "Temp",
    str_detect(Variable, "Precip") ~ "Precip",
    TRUE ~ as.character(Variable)
  )) %>%
  
  # calc median importance of each group by frr
  group_by(group, frr) %>%
  summarise(Importance = mean(Importance, na.rm = T)) %>%
  
  filter(!is.nan(Importance)) %>%
  
  arrange(group,frr) %>%
  
  
  group_by(group) %>%
  mutate(imp_med = median(Importance)) %>%
  
  ungroup() %>%
  
  # add custom html tags to group labels;
  # order group by importance
  mutate(group = case_when(
    group %in% nolte2020vars ~ paste0("<span style='color:Grey;'>", 
                                      group,
                                      "</span>"),
    TRUE ~ paste0("<span style='font-weight:1000;'>", group, "</span>")),
    group = fct_reorder(group, imp_med))

# VIZ ####
## Full FRR Importance Plot ####

ffb_imp_clean  %>%
  
  ggplot(aes(Importance,
             group, colour = frr)
         ) +
  
  geom_hline(yintercept = 0) +
  
  geom_point() +
  
  #stat_boxplot(geom = "errorbar",
  #             size = 0.2) +
  #geom_boxplot(alpha = 1, size = 0.2, 
  #             colour = "black", outlier.size = 0.6,
  #             outlier.colour = "grey50") +
  
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(0, 1.5, by = 0.25)) +
  scale_y_discrete(expand = c(0.03,0.03)) +
  scale_colour_manual(
    values = frr_colors
  ) +
  
  labs(
    title = "Feature Importance by Farm Resource Region",
    subtitle = 
      "Permutation feature importance. Features added by this paper denoted in **bold**.",
    caption = "Climatic variables grouped for clarity",
    x = "Feature Importance",
    y = NULL,
    colour = "Farm Resource Region") +
  
  theme(
    text = element_text(family = font, size = 25),
    axis.ticks = element_blank(),
    axis.text.y = ggtext::element_markdown(size = 10),
    axis.title.x = element_text(size = 20),
    legend.key = element_blank(),
    legend.title = element_text(size = 19),
    panel.background = element_blank(),
    #panel.border = element_rect(fill = NA, colour = "black"),
    panel.grid.major.y = element_line(size = 0.1, colour = "grey70"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face= "bold"),
    plot.subtitle = element_markdown(size = 20),
    plot.caption = element_text(size = 15, face = "italic"),
    plot.margin = margin(rep(15, 4))
  )

#### FRR Climate Importance Plots ####
importance_frr_clean %>%
  
  filter(str_detect(group, "(Temp|Precip)"),
         !is.nan(Importance)) %>%
  mutate(Variable = str_remove_all(Variable,"(Precip_|DewTempMean_|^Temp)"),
         Variable = str_to_title(Variable)) %>%
  
  
  ggplot(aes(Importance, Variable))+
  
  geom_point(aes(colour = frr), 
             size = 2.5, alpha = 0.9)+
  #geom_boxplot(outlier.shape = NA)+
  
  scale_x_continuous(limits = c(0,NA))+
  
  scale_colour_manual(
    values = frr_colors
  ) +
  
  facet_wrap(~group, scales = "free_y")+
  
  labs(
    title = 'Feature Importance by Farm Resource Region: Climate',
    subtitle = 'Permutation importance. Farm resource region full model.',
    x = "Feature Importance",
    y = NULL,
    colour = "Farm Resource Region")+
  
  theme(
    text = element_text(family = font, size = 18),
    strip.text = element_text(face = "italic", size = 16),
    strip.background = element_blank(),
    axis.ticks.y = element_line(size = 0.5),
    axis.title.x = element_text(face = "bold"),
    legend.key = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(rep(15,4)),
    panel.grid.major.y = element_line(colour = "grey70"),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black")
  )