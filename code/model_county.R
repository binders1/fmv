library(MASS)
library(broom)

# Load County Adjacency df ####
county_adjacency <- 
  read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv")

state_counties <- 
  get(df_final) %>%
  pull(fips) %>%
  unique()

# Specify model stats df to be grown later ####
collect_stats <- 
  tibble(stat = c("r.squared", "adj.r.squared", "sigma",
                  "statistic", "p.value", "df", "logLik",
                  "AIC", "BIC", "deviance", "df.residual",
                  "nobs", "fips")) %>% 
  mutate(na = NA) %>%
  pivot_wider(
    names_from = stat,
    values_from = na) %>%
  slice(0)


# Loop through all states ####

for (i in seq_len(length(state_counties))) {
  
  neighbors <- county_adjacency %>%
    dplyr::filter(countyname != neighborname) %>%  
    dplyr::filter(fipscounty == state_counties[[i]]) %>%
    pull(fipsneighbor)
  
  county_df <- get(df_final) %>%
    dplyr::select(price, fips, any_of(nolte2020vars)) %>% # select model vars
    dplyr::filter(fips==state_counties[[i]]) 
  
  
  if (nrow(county_df) < 1000) {
    
    j<-1
    
    while(nrow(county_df)<1000 & j <= length(neighbors)) {
      
      neighbor_df <- get(df_final) %>%
        dplyr::select(price, fips, any_of(nolte2020vars)) %>%
        dplyr::filter(fips==neighbors[[j]])
      
      county_df <- bind_rows(county_df, neighbor_df)
      
      j <- j+1
      
      if(nrow(county_df)>=1000 | j > length(neighbors)) {
        break
      }
      
    }
  }
  
  if(nrow(county_df)>=1000) {
    
    ## Modeling ####
    
    ### Regression ####
    
    stepMod <- stepAIC(lm(price ~ ., 
                          data = county_df %>%
                            na.omit() %>%
                            dplyr::select(!fips)
                            ), 
                       trace = FALSE, 
                       direction = "both")
    
    #### Collect R-squared #### 
    county_stats <- glance(stepMod) %>%
      mutate(fips = state_counties[[i]])
    
    #### Bind current county stats to stats df ####
    collect_stats <- collect_stats %>%
      rbind(county_stats)
    
    cat("Complete: ", state_counties[[i]], 
        " |.....| Modeled: YES, n.obs: ", nrow(county_df), "\n",
        sep = "")
    
  } else {
    
    cat("Complete: ", state_counties[[i]], 
        " |.....| Modeled: NO, n.obs: ", nrow(county_df), "\n",
        sep = "")
  
    }

}


# Plot Predictive Power ####


noltecolors <- c('#FBFDD0', '#40B5C4','#081D59')

usmap::plot_usmap(
  data = collect_stats,
  values = "r.squared",
  include = "Arkansas",
  colour = "grey92",
  size = 0.2)+
  scale_fill_gradientn(
    name = "R-squared",
    colours = noltecolors,
    na.value = "lightgrey")+
  labs(
    title = "Predictive Power by County",
    subtitle = "subtitle"
  )+
  theme(
    legend.position = "left",
    legend.background = element_blank(),
    text = element_text(size = 17, family = "IBM Plex Sans")
  )

