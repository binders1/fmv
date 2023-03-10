# ==================================
# Download PNAS parcel predictions
# ==================================

# Google Drive Connection
if (!drive_has_token()) drive_auth("gold1@roar.stolaf.edu")

drive_id_nolte_workshop <- 
  as_id("1eiEu-XFLjThzTZ7nU2s8rb54SWQRyuti")

# Retrieve all state folders in workshop parcels/ folder
pnas_states <- 
  drive_ls(drive_id_nolte_workshop)

# Retrieve all county folders and bind into a single dataframe
pnas_county_folders <-
  seq_len(nrow(pnas_states)) %>%
  map(~drive_ls(slice(pnas_states, .x))) %>%
  bind_rows()

# Within each county folder, retrieve the pnas parcel predictions file
filter_to_pnas <- function(data, index) {
  slice(data, index) %>%
    drive_ls() %>%
    filter(str_detect(name, "_pnas2020_pqt.zip$"))
}

pnas_parcel_pred <-
  pnas_county_folders %>%
  nrow() %>%
  seq_len() %>%
  map(~filter_to_pnas(pnas_county_folders, .x)) %>%
  bind_rows() 

# Download each zipped pnas parcel prediction file
walk(
  pnas_parcel_pred$name,
  ~ drive_download(.x, 
                   path = file.path(z.dir, .x),
                   overwrite = TRUE)
)

zipped_files <- list.files(z.dir, pattern = "pnas2020")

# Unzip all pnas prediction files
walk(
  zipped_files,
  ~ unzip(zipfile = file.path(z.dir, .x),
          exdir = file.path(ddir, "pnas2020_parcel"))
  )
