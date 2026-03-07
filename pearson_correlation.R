library(terra)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)

input_dir <- "D:/미국연구2월리트라이/MaxEnt_input/34var/VIF10"
asc_files <- list.files(input_dir, pattern = "\\.asc$", full.names = TRUE)

rasters <- rast(asc_files)
raster_df <- as.data.frame(rasters, na.rm = TRUE)

cor_matrix <- cor(raster_df, use = "pairwise.complete.obs")
cor_matrix_rounded <- round(cor_matrix, 3)

wb <- createWorkbook()
addWorksheet(wb, "Pearson_r")

writeData(wb, sheet = "Pearson_r", x = cor_matrix_rounded, rowNames = TRUE)

output_file <- "D:/미국연구2월리트라이/correlation_results.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)
