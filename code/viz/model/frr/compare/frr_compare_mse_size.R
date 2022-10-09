
# Load Font
font <- "Open Sans"
loadFont(font)


# Get vector of <1000 obs counties ####
ncb_size_cat <-
  loadResults(model = "ncb", res_type = "performance") %>%
  dplyr::mutate(
    size_cat = if_else((n_obs - n_neighbor) <= 1000,
                       "u1k",
                       "g1k"
                       )
    ) %>%
  select(fips, size_cat)


# Load predictions ####

mods_to_load <- 
  c("ffb" = "ffb", "nfb" = "nfb")

frr_predictions <-
  map(mods_to_load, 
      ~ loadResults(model = .x, res_type = "predictions"))

# Subset to only <1000 obs counties ####
frr_size_cat_pred <-
  data.table::rbindlist(frr_predictions, fill = T) %>%
  left_join(ncb_size_cat,
            by = "fips")

# Calculate mse by model (ffb vs. nfb) ####
frr_size_cat_mse <- 
  frr_size_cat_pred %>%
  mutate(sq_error = (log_priceadj_ha - .pred)^2,
         model = if_else(model == "ffb", "Full", "Nolte")) %>%
  group_by(model, fips) %>%
  mutate(mse = mean(sq_error)) %>%
  ungroup() %>%
  select(size_cat, model, id, fips, mse) %>%
  distinct() %>%
  mutate(type = paste0(model, " ", size_cat),
         type = case_when(
           str_detect(type, "^N.+u") ~ "Nolte, n < 1,000",
           str_detect(type, "^N.+g") ~ "Nolte, n > 1,000",
           str_detect(type, "^F.+u") ~ "Full, n < 1,000",
           str_detect(type, "^F.+g") ~ "Full, n > 1,000",
           TRUE ~ as.character(type)
         )) %>%
  na.omit()



# Plot performance by model ####
frr_size_cat_mse %>%
  ggplot(aes(mse, type, fill = type)) +
  
  geom_vline(xintercept = 0) +
  
  stat_boxplot(geom = "errorbar", 
               width = 0.1) +  
  geom_boxplot(width = 0.3, 
               outlier.colour = NA,
               colour = "black") +
  
  scale_x_continuous(limits = c(0,2.5),
                     expand = c(0,NA)) +
  
  scale_fill_manual(
    values = brewer.pal(4, "Paired")
  ) +
  
  labs(
    x = "\nMean Squared Error",
    y = NULL
  ) +
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 24, family = font),
        plot.background = element_blank(),
        plot.margin = margin(rep(25, 4)),
        legend.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90"),
        strip.background = element_blank()
  )




# Average MSE nad R^2
frr_size_cat_mse %>%
  filter(model == "Full") %>%
  group_by(size_cat) %>%
  summarise(avg_mse = mean(mse, na.rm = TRUE))


