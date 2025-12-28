library(terra)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)  # 엑셀 쓰기용 패키지

# 🔹 ASC 파일 경로
input_dir <- "C:/Users/김연찬/Documents/output2"
asc_files <- list.files(input_dir, pattern = "\\.asc$", full.names = TRUE)

# 🔹 모든 ASC 파일을 하나의 SpatRaster로 불러오기
rasters <- rast(asc_files)

# 🔹 각 raster를 벡터로 변환하여 데이터프레임 생성
raster_df <- as.data.frame(rasters, na.rm = TRUE)

# 🔹 상관행렬 (Pearson correlation)
cor_matrix <- cor(raster_df, use = "pairwise.complete.obs")

# 🔹 1 / (1 - r^2) 계산 행렬
vif_like_matrix <- 1 / (1 - cor_matrix^2)

# 🔹 소수점 반올림
cor_matrix_rounded <- round(cor_matrix, 3)
vif_matrix_rounded <- round(vif_like_matrix, 3)

# 🔹 엑셀 워크북 생성 및 시트 추가
wb <- createWorkbook()
addWorksheet(wb, "Pearson_r")
addWorksheet(wb, "VIF_1/(1-r^2)")

writeData(wb, sheet = "Pearson_r", x = cor_matrix_rounded, rowNames = TRUE)
writeData(wb, sheet = "VIF_1/(1-r^2)", x = vif_matrix_rounded, rowNames = TRUE)

# 🔹 엑셀로 저장
output_file <- "C:/Users/김연찬/Documents/correlation_vif_results.xlsx"
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("✅ 상관계수 및 VIF 행렬이 엑셀로 저장되었습니다:\n", output_file, "\n")
