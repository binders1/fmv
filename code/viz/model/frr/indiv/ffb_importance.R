# Load packages ####



## Load Nolte (2020) features
root <- "~/fmv"
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
odir <- file.path(root, "output")
noltevars_path <- file.path(ddir, "nolte2020vars.csv")

nolte2020vars <- 
  read_csv(noltevars_path,
           show_col_types = F) %>% 
  pull() %>%
  c(., "ha", "x45", "y45")

# Source Prep ####
source(file.path(cdir, "functions/sourceFuncs.R"))
sourceFuncs()
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(cdir, "viz/model/frr/viz_frr_prep.R"))

# Load FRR importance (ffb) ####
ffb_imp <-
  loadResults("ffb", "importance")


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
    str_detect(Variable, "^Dew") ~ "Dew Temperature",
    str_detect(Variable, "(?<!Dew)Temp") ~ "Temperature",
    str_detect(Variable, "Precip") ~ "Precipitation",
    Variable == "final_mhv" ~ "Median Home Value",
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

## Top N FRR Importance Plot ####

ffb_imp_clean %>%
  
  arrange(-imp_med) %>%
  group_by(frr) %>%
  slice(1:20) %>%
  ungroup() %>%
  
  ggplot(
    aes(Importance, group, colour = frr)
    ) +

  geom_hline(yintercept = 0) +
  
  geom_point(size = 2.5) +
  
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(0, 1.5, by = 0.25)) +
  scale_y_discrete(expand = c(0.03,0.03)) +
  scale_colour_manual(
    values = frr_colors
  ) +
  
  labs(
    x = "Feature Importance",
    y = NULL,
    colour = "Farm Resource Region") +
  
  theme(
    text = element_text(family = "sans", size = 16),
    axis.ticks = element_blank(),
    axis.text.y = ggtext::element_markdown(),
    axis.title.x = element_text(size = 14),
    legend.key = element_blank(),
    legend.title = element_text(size = 14),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = "grey70"),
    panel.grid.major.x = element_blank(),
    )




## Full FRR Importance Plot ####

ffb_imp_clean  %>%
  
  ggplot(aes(Importance,
             group, colour = frr)
  ) +
  
  geom_hline(yintercept = 0) +
  
  geom_point() +
  
  scale_x_continuous(expand = c(0.01,0), 
                     breaks = seq(0, 1.5, by = 0.25)) +
  scale_y_discrete(expand = c(0.03,0.03)) +
  scale_colour_manual(
    values = frr_colors
  ) +
  
  labs(
    x = "Feature Importance",
    y = NULL,
    colour = "Farm Resource Region") +
  
  theme(
    text = element_text(family = "sans", size = 13),
    axis.ticks = element_blank(),
    axis.text.y = ggtext::element_markdown(size = 9),
    axis.title.x = element_text(size = 17),
    legend.key = element_blank(),
    legend.title = element_text(size = 13),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, colour = "grey70"),
    panel.grid.major.x = element_blank(),
    )

