


predict_all_pc <-
  file.path(m.dir, "model_all_parcels") %>%
  list.files(full.names = TRUE)

  load_pred_all(
    predict_all_pc[3],
    pid, .pred, x, y
  ) %>%
    rowid_to_column(var = "pidrow")
