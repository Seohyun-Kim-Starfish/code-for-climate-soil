# 1. 필수 패키지 로드
if(!require(terra)) install.packages("terra")
if(!require(usdm)) install.packages("usdm")
if(!require(dplyr)) install.packages("dplyr")

library(terra)
library(usdm)
library(dplyr)

# 2. 경로 설정 및 파일 필터링
input_dir <- "C:/Users/user/Desktop/Assessing MaxEnt’s Predictive Performance Under Local Data Exclusion/MaxEnt_input/34var"
all_files <- list.files(input_dir, pattern = "\\.asc$", full.names = TRUE)

exclude_files <- c("sumbases_r.asc", "ecec_r.asc", "cec7_r.asc","ny_mean_annual_et_2015_2024_processed.asc","ny_mean_annual_et_2015_2024_processed_epsg4326.asc","nlcd_ny_epsg4326.asc")
target_files <- all_files[!(basename(all_files) %in% exclude_files)]

# 3. 데이터 로드
rasters <- rast(target_files)

# --- [분석 실행] ---

# 1) 초기 모든 변수의 VIF 계산
vif_init <- vif(rasters)
colnames(vif_init) <- c("Variables", "Initial_VIF")

# 2) VIF 10 기준 단계적 선택
vs10 <- vifstep(rasters, th = 10)
vif_10_res <- as.data.frame(vs10@results)
colnames(vif_10_res) <- c("Variables", "VIF_Thresh10")

# 3) VIF 5 기준 단계적 선택
vs5 <- vifstep(rasters, th = 5)
vif_5_res <- as.data.frame(vs5@results)
colnames(vif_5_res) <- c("Variables", "VIF_Thresh5")

# --- [데이터 통합] ---

final_report <- vif_init %>%
  left_join(vif_10_res, by = "Variables") %>%
  left_join(vif_5_res, by = "Variables")

output_file <- "C:/Users/user/Desktop/VIF_Comparison_Summary.csv"
write.csv(final_report, output_file, row.names = FALSE, na = "")
cat("파일 경로:", output_file, "\n")