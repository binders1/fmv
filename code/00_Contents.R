#### STEPS ####
getwd()
setwd('code')

# 0. Build custom functions
source('custom_functions')

# 1. Load parcel pqt files from ArcResults
#     a. Also load soilcodes
source('import_ArcResults')

#     b. Clean soil, irrigation, and climate variables
#         - Soil
#         - Irrigation
#         - Temp
#         - Dew
# 2. Load sales pqt data from data folder
#     

# 3. Load sale_pid crosswalk from data folder

# 4. Merge sales to crosswalk to parcel pqt
#       For 2-4, import_Nolte.R specifies all_sale and all_salepid

# 5. Subset to single county
#     a. First, create full grid template to join with actual data
#     b. If <1000 obs, retrieve nec obs from neighboring counties

county_adjacency <- read_csv("https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv")


neighbors <- county_adjacency %>%
  filter(countyname != neighborname) %>%  
  filter(fipscounty == "01001") %>%
  pull(fipsneighbor)

# 6. Select optimal model with AIC
# 7. Run model
# 8. Collect R-sq
# 9. Move on to next county
#
# 10. When state is finished, move on to next state with steps 1-9