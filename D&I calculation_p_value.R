install.packages("tools") # tools 패키지가 없다면 설치
library(terra)
library(tools)

# 2. 경로 설정
# --- 사용자가 직접 설정해야 하는 부분 ---
base_raster_path <- "C:/Users/김연찬/Documents/maxent_output/19final_all/Japanese_knotweed.asc"
compare_folder_path <- "C:/Users/김연찬/Documents/maxent_output/asc of '-'"
shp_folder_path <- "C:/Users/김연찬/Documents/NY_droughtregion_shp"
output_csv_path <- "C:/Users/김연찬/Documents/cropped_niche_overlap_results.csv" # 결과 CSV 파일 저장 경로

# 3. 비교 대상 ASC 파일 목록 가져오기
all_asc_files <- list.files(
  path = compare_folder_path,
  pattern = "\\.asc$",
  full.names = TRUE
)

# 4. 결과를 저장할 데이터 프레임 초기화
results_df <- data.frame()

# 5. 기본 래스터 로드
base_raster <- terra::rast(base_raster_path)
message("기본 래스터 로드 완료: ", basename(base_raster_path))

# 6. 메인 루프: 각 asc 파일에 대해 분석 시작
message("\n--- 지역별(Shapefile) 래스터 잘라내기 및 분석을 시작합니다 ---")

for (asc_file in all_asc_files) {
  
  # 7. 비교 asc 파일명에서 shapefile 이름 추출 및 경로 생성
  # 예: "-adirondack.asc" -> "adirondack"
  shp_name_key <- sub("^-", "", tools::file_path_sans_ext(basename(asc_file)))
  shp_file_path <- file.path(shp_folder_path, paste0(shp_name_key, ".shp"))
  
  cat("\nProcessing:", basename(asc_file))
  
  # 8. 매칭되는 shapefile이 존재하는지 확인
  if (!file.exists(shp_file_path)) {
    warning(" -> 매칭되는 Shapefile 없음: ", basename(shp_file_path), ". 건너뜁니다.")
    next # 다음 파일로 넘어감
  }
  
  # 9. 에러 발생 시에도 전체 스크립트가 중단되지 않도록 tryCatch 사용
  tryCatch({
    # 10. 데이터 로드 (비교 래스터 및 shapefile)
    compare_raster <- terra::rast(asc_file)
    region_shp <- terra::vect(shp_file_path)
    
    # 11. CRS(좌표계) 통일: shp 파일의 좌표계를 래스터 기준으로 변환
    region_shp_proj <- terra::project(region_shp, terra::crs(base_raster))
    
    # 12. 래스터 자르기 (Crop & Mask)
    base_raster_cropped <- terra::mask(terra::crop(base_raster, region_shp_proj), region_shp_proj)
    compare_raster_cropped <- terra::mask(terra::crop(compare_raster, region_shp_proj), region_shp_proj)
    
    # 13. 리샘플링
    compare_raster_resampled <- terra::resample(compare_raster_cropped, base_raster_cropped, method = "bilinear")
    
    # 14. 값 추출 및 유효성 검사 (NA 값 제외)
    base_vals <- terra::values(base_raster_cropped)
    compare_vals <- terra::values(compare_raster_resampled)
    valid_indices <- which(!is.na(base_vals) & !is.na(compare_vals))
    
    # 잘라낸 지역에 유효한 셀이 하나도 없는 경우 처리
    if (length(valid_indices) == 0) {
      warning(" -> 잘린 지역 내에 유효한 공통 데이터가 없어 계산을 건너뜁니다.")
      next
    }
    
    base_vals_valid <- base_vals[valid_indices]
    compare_vals_valid <- compare_vals[valid_indices]
    
    # 15. 확률 분포로 정규화
    base_probs <- base_vals_valid / sum(base_vals_valid)
    compare_probs <- compare_vals_valid / sum(compare_vals_valid)
    
    # 16. Schoener’s D 및 Warren’s I 계산
    schoeners_d <- 1 - 0.5 * sum(abs(base_probs - compare_probs))
    warrens_i <- 1 - 0.5 * sqrt(sum((sqrt(base_probs) - sqrt(compare_probs))^2))
    
    # 17. 결과 데이터 프레임에 추가
    results_df <- rbind(results_df, data.frame(
      ComparisonFile = basename(asc_file),
      RegionShapefile = basename(shp_file_path),
      Schoeners_D = schoeners_d,
      Warrens_I = warrens_i
    ))
  }, error = function(e) {
    message("\n에러 발생: ", basename(asc_file), " - ", e$message)
  }) # tryCatch 끝
}

# 18. 최종 결과 출력 및 CSV 파일로 저장
print(results_df)

write.csv(results_df, output_csv_path, row.names = FALSE, fileEncoding = "UTF-8")


#-------------------------------------------------------------------------------------------
library(terra)
library(tools)

# 2. 경로 설정
# --- 사용자가 직접 설정해야 하는 부분 ---
base_raster_path <- "C:/Users/김연찬/Documents/maxent_output/19final_all/Japanese_knotweed.asc"
compare_folder_path <- "C:/Users/김연찬/Documents/maxent_output/asc of '-'"
shp_folder_path <- "C:/Users/김연찬/Documents/NY_droughtregion_shp"
output_csv_path <- "C:/Users/김연찬/Documents/cropped_niche_overlap_통계분석.csv" # 결과 CSV 파일 저장 경로

# 3. 비교 대상 ASC 파일 목록 가져오기
all_asc_files <- list.files(
  path = compare_folder_path,
  pattern = "\\.asc$",
  full.names = TRUE
)

# 4. 결과를 저장할 데이터 프레임 초기화
results_df <- data.frame()

# 5. 기본 래스터 로드
base_raster <- terra::rast(base_raster_path)
message("기본 래스터 로드 완료: ", basename(base_raster_path))

# 6. 메인 루프: 각 asc 파일에 대해 분석 시작
message("\n--- 지역별(Shapefile) 래스터 잘라내기 및 분석을 시작합니다 ---")

for (asc_file in all_asc_files) {
  
  # 7. 비교 asc 파일명에서 shapefile 이름 추출 및 경로 생성
  shp_name_key <- sub("^-", "", tools::file_path_sans_ext(basename(asc_file)))
  shp_file_path <- file.path(shp_folder_path, paste0(shp_name_key, ".shp"))
  
  cat("\nProcessing:", basename(asc_file))
  
  # 8. 매칭되는 shapefile이 존재하는지 확인
  if (!file.exists(shp_file_path)) {
    warning(" -> 매칭되는 Shapefile 없음: ", basename(shp_file_path), ". 건너뜁니다.")
    next
  }
  
  # 9. 에러 발생 시에도 전체 스크립트가 중단되지 않도록 tryCatch 사용
  tryCatch({
    # 10. 데이터 로드
    compare_raster <- terra::rast(asc_file)
    region_shp <- terra::vect(shp_file_path)
    
    # 11. CRS(좌표계) 통일
    region_shp_proj <- terra::project(region_shp, terra::crs(base_raster))
    
    # 12. 래스터 자르기 (Crop & Mask)
    base_raster_cropped <- terra::mask(terra::crop(base_raster, region_shp_proj), region_shp_proj)
    compare_raster_cropped <- terra::mask(terra::crop(compare_raster, region_shp_proj), region_shp_proj)
    
    # 13. 리샘플링
    compare_raster_resampled <- terra::resample(compare_raster_cropped, base_raster_cropped, method = "bilinear")
    
    # 14. 값 추출 및 유효성 검사
    base_vals <- terra::values(base_raster_cropped)
    compare_vals <- terra::values(compare_raster_resampled)
    valid_indices <- which(!is.na(base_vals) & !is.na(compare_vals))
    
    if (length(valid_indices) < 2) { # 유효한 셀이 너무 적으면 건너뛰기
      warning(" -> 유효한 공통 데이터가 부족하여 계산을 건너뜁니다.")
      next
    }
    
    base_vals_valid <- base_vals[valid_indices]
    compare_vals_valid <- compare_vals[valid_indices]
    
    # 정규화 과정에서 합계가 0이 되는 경우 방지
    if (sum(base_vals_valid) == 0 || sum(compare_vals_valid) == 0) {
      warning(" -> 래스터 값의 합이 0이므로 계산을 건너뜁니다.")
      next
    }
    
    # 15. 확률 분포로 정규화
    base_probs <- base_vals_valid / sum(base_vals_valid)
    compare_probs <- compare_vals_valid / sum(compare_vals_valid)
    
    # 16. Schoener’s D 및 Warren’s I 계산
    # (변수 이름을 원래대로 schoeners_d, warrens_i 로 되돌렸습니다)
    schoeners_d <- 1 - 0.5 * sum(abs(base_probs - compare_probs))
    warrens_i <- 1 - 0.5 * sqrt(sum((sqrt(base_probs) - sqrt(compare_probs))^2))
    
    # --- 통계적 유의성 검증 (순열 검정) ---
    n_permutations <- 9999
    null_d_scores <- numeric(n_permutations)
    null_i_scores <- numeric(n_permutations)
    
    combined_vals <- c(base_vals_valid, compare_vals_valid)
    
    for (i in 1:n_permutations) {
      shuffled_vals <- sample(combined_vals)
      perm_base_vals <- shuffled_vals[1:length(base_vals_valid)]
      perm_compare_vals <- shuffled_vals[(length(base_vals_valid) + 1):length(shuffled_vals)]
      
      # 순열 검정 중 합계가 0이 되는 경우 방지
      if(sum(perm_base_vals) == 0 || sum(perm_compare_vals) == 0) {
        null_d_scores[i] <- NA # 오류 대신 NA로 처리
        null_i_scores[i] <- NA
        next
      }
      
      perm_base_probs <- perm_base_vals / sum(perm_base_vals)
      perm_compare_probs <- perm_compare_vals / sum(perm_compare_vals)
      
      null_d_scores[i] <- 1 - 0.5 * sum(abs(perm_base_probs - perm_compare_probs))
      null_i_scores[i] <- 1 - 0.5 * sqrt(sum((sqrt(perm_base_probs) - sqrt(perm_compare_probs))^2))
    }
    
    # NA가 된 값을 제외하고 p-value 계산
    p_value_d <- (sum(null_d_scores >= schoeners_d, na.rm = TRUE) + 1) / (sum(!is.na(null_d_scores)) + 1)
    p_value_i <- (sum(null_i_scores >= warrens_i, na.rm = TRUE) + 1) / (sum(!is.na(null_i_scores)) + 1)
    
    # 17. 결과 데이터 프레임에 추가
    results_df <- rbind(results_df, data.frame(
      ComparisonFile = basename(asc_file),
      RegionShapefile = basename(shp_file_path),
      Schoeners_D = schoeners_d,
      Warrens_I = warrens_i,
      p_value_D = p_value_d,
      p_value_I = p_value_i
    ))
    
  }, error = function(e) {
    message("\n에러 발생: ", basename(asc_file), " - ", e$message)
  }) # tryCatch 끝
}

# 18. 최종 결과 출력 및 CSV 파일로 저장
print(results_df)

write.csv(results_df, output_csv_path, row.names = FALSE, fileEncoding = "UTF-8")

