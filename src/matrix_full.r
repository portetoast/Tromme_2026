matrix_final <- matrix_full_eco_elev_clim_sat

head(matrix_final)

write.csv(
  matrix_final,
  "matrix_final.csv",
  row.names = FALSE
)