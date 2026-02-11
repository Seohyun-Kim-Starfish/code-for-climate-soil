library(terra)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)

input_dir <- "C:/Users/김연찬/Documents/output2"
asc_files <- list.files(input_dir, pattern = "\\.asc$", full.names = TRUE)

rasters <- rast(asc_files)
raster_df <- as.data.frame(rasters, na.rm = TRUE)

cor_matrix <- cor(raster_df, use = "pairwise.complete.obs")
vif_like_matrix <- 1 / (1 - cor_matrix^2)

cor_matrix_rounded <- round(cor_matrix, 3)
vif_matrix_rounded <- round(vif_like_matrix, 3)

wb <- createWorkbook()
addWorksheet(wb, "Pearson_r")
addWorksheet(wb, "VIF_1/(1-r^2)")

writeData(wb, sheet = "Pearson_r", x = cor_matrix_rounded, rowNames = TRUE)
writeData(wb, sheet = "VIF_1/(1-r^2)", x = vif_matrix_rounded, rowNames = TRUE)

output_file <- "C:/Users/김연찬/Documents/correlation_vif_results.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)
