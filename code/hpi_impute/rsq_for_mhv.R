medhomeval <-
  read_parquet("~/fmv/data/hpi_impute/realtor/medhomeval.pqt")


medval_missing <-
  medhomeval %>%
  filter(fips %in% fips_missing_hpi)


buffer <- 
  read_parquet("~/fmv/data/hpi_impute/hpi_buffer.pqt")

buffer %<>%
  filter(fips %in% fips_missing_hpi,
         fips != buffer_fips)


val_buffer_join <-
  medval_missing %>%
  select(!year) %>%
  left_join(
    buffer,
    by = "fips"
  ) %>%
  mutate(inv_sq_dist = 1/(dist_m^2), .keep = "unused") %>%
  rename(mhv_focal = "median_listing_price") %>%
  left_join(
    medhomeval %>% select(!year),
    by = c("buffer_fips" = "fips")
  ) %>%
  rename(mhv_buffer = "median_listing_price")


val_buffer_join %>%
  group_by(fips) %>%
  mutate(pred = weighted.mean(mhv_buffer, inv_sq_dist)) %>%
  #mutate(pred = mean(mhv_buffer)) %>%
  ungroup() %>%
  select(fips, mhv_focal, pred) %>%
  distinct() %>%
  mutate(mhv_mean = mean(mhv_focal),
         actual_diff_sq = (mhv_focal - mhv_mean)^2,
         sq_error = (mhv_focal - pred)^2) %>%
  summarise(sst = sum(actual_diff_sq),
            ssr = sum(sq_error, na.rm = T)) %>%
  transmute(R2 = 1 - (ssr/sst))


