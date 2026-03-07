library(terra)
library(tools)

# 2. 경로 설정
base_raster_path <- "D:/미국연구2월리트라이/MaxEnt_result/final/Japanese_knotweed.asc"
compare_folder_path <- "D:/미국연구2월리트라이/MaxEnt_result/final/asc of '-'"
shp_folder_path <- "C:/Users/김연찬/Documents/NY_droughtregion_shp"
output_csv_path <- "C:/Users/김연찬/Documents/cropped_niche_overlap_통계분석.csv" 

# 3. 대상 파일 목록 가져오기
all_asc_files <- list.files(path = compare_folder_path, pattern = "\\.asc$", full.names = TRUE)
all_shp_files <- list.files(path = shp_folder_path, pattern = "\\.shp$", full.names = TRUE)

# --- 파일명 정규화 함수 ---
# 대소문자 무시, 공백/하이픈/언더바 제거하여 텍스트만 남김 (유연한 매칭용)
normalize_string <- function(x) {
  x <- tools::file_path_sans_ext(x)
  x <- tolower(x)
  x <- gsub("[ \\-_]", "", x)
  return(x)
}

# Shapefile 이름들을 미리 정규화하여 매칭 리스트 생성
shp_normalized_names <- sapply(basename(all_shp_files), normalize_string)

# 4. 결과를 저장할 데이터 프레임 초기화
results_df <- data.frame()

# 5. 기본 래스터 로드
base_raster <- terra::rast(base_raster_path)
message("기본 래스터 로드 완료: ", basename(base_raster_path))

# 6. 메인 루프: 각 asc 파일에 대해 분석 시작
message("\n--- 지역별(Shapefile) 래스터 잘라내기 및 분석을 시작합니다 ---")

for (asc_file in all_asc_files) {
  asc_basename <- basename(asc_file)
  
  # --- [수정된 핵심 부분] ASC 파일명에서 맨 앞 '-' 제거 후 SHP와 매칭 ---
  
  # 1) asc 파일명 맨 앞에 '-'가 있다면 제거 (예: "-Adirondack.asc" -> "Adirondack.asc")
  clean_asc_name <- sub("^-", "", asc_basename) 
  
  # 2) 파일명 정규화 (대소문자, 공백 등 무시)
  asc_norm <- normalize_string(clean_asc_name)
  
  # 3) 정규화된 이름으로 SHP 파일 찾기
  matched_idx <- match(asc_norm, shp_normalized_names)
  
  cat("\nProcessing:", asc_basename)
  
  # 매칭되는 파일이 없는 경우 건너뛰기
  if (is.na(matched_idx)) {
    warning(" -> 매칭되는 Shapefile을 찾을 수 없습니다 (기준 이름: ", clean_asc_name, "). 건너뜁니다.")
    next
  }
  
  # 정확한 Shapefile 경로 할당
  shp_file_path <- all_shp_files[matched_idx]
  
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
    
    if (length(valid_indices) < 2) {
      warning(" -> 유효한 공통 데이터가 부족하여 계산을 건너뜁니다.")
      next
    }
    
    base_vals_valid <- base_vals[valid_indices]
    compare_vals_valid <- compare_vals[valid_indices]
    
    if (sum(base_vals_valid) == 0 || sum(compare_vals_valid) == 0) {
      warning(" -> 래스터 값의 합이 0이므로 계산을 건너뜁니다.")
      next
    }
    
    # 15. 확률 분포로 정규화
    base_probs <- base_vals_valid / sum(base_vals_valid)
    compare_probs <- compare_vals_valid / sum(compare_vals_valid)
    
    # 16. Schoener’s D 및 Warren’s I 계산
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
      
      if(sum(perm_base_vals) == 0 || sum(perm_compare_vals) == 0) {
        null_d_scores[i] <- NA
        null_i_scores[i] <- NA
        next
      }
      
      perm_base_probs <- perm_base_vals / sum(perm_base_vals)
      perm_compare_probs <- perm_compare_vals / sum(perm_compare_vals)
      
      null_d_scores[i] <- 1 - 0.5 * sum(abs(perm_base_probs - perm_compare_probs))
      null_i_scores[i] <- 1 - 0.5 * sqrt(sum((sqrt(perm_base_probs) - sqrt(perm_compare_probs))^2))
    }
    
    p_value_d <- (sum(null_d_scores >= schoeners_d, na.rm = TRUE) + 1) / (sum(!is.na(null_d_scores)) + 1)
    p_value_i <- (sum(null_i_scores >= warrens_i, na.rm = TRUE) + 1) / (sum(!is.na(null_i_scores)) + 1)
    
    # 17. 결과 데이터 프레임에 추가
    results_df <- rbind(results_df, data.frame(
      ComparisonFile = asc_basename,
      RegionShapefile = basename(shp_file_path),
      Schoeners_D = schoeners_d,
      Warrens_I = warrens_i,
      p_value_D = p_value_d,
      p_value_I = p_value_i
    ))
    
  }, error = function(e) {
    message("\n에러 발생: ", asc_basename, " - ", e$message)
  })
}

# 18. 최종 결과 출력 및 CSV 파일로 저장
print(results_df)
write.csv(results_df, output_csv_path, row.names = FALSE, fileEncoding = "UTF-8")