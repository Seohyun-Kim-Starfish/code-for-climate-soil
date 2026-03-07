library(data.table)
library(terra)

# 1. 설정 및 메모리 옵션
# terra가 대용량 파일을 처리할 때 임시 디스크 공간을 사용하도록 유도합니다.
terraOptions(memfrac = 0.6, tempdir = "D:/미국연구2월리트라이/temp")
if(!dir.exists("D:/미국연구2월리트라이/temp")) dir.create("D:/미국연구2월리트라이/temp")

# 2. 경로 설정
base_dir <- "D:/미국연구2월리트라이/MaxEnt_input"
ref_path <- file.path(base_dir, "pearson/BIO3_bio_variable_processed_epsg4326.asc")
occ_path <- file.path(base_dir, "occurrence.txt")

# 3. 참조 래스터(BIO3) 로드 (헤더 정보만 읽어 메모리 절약)
ref_raster <- rast(ref_path)

# 4. 좌표 데이터 읽기 (좌표만 추출)
header <- fread(occ_path, nrows = 5, quote = "")
cols <- names(header)
target_lon <- cols[tolower(cols) %in% c("decimallongitude", "longitude", "long", "lon", "x")][1]
target_lat <- cols[tolower(cols) %in% c("decimallatitude", "latitude", "lat", "y")][1]

occ_data <- fread(occ_path, select = c(target_lon, target_lat), quote = "")
setnames(occ_data, c(target_lon, target_lat), c("lon", "lat"))
occ_data <- na.omit(occ_data)
occ_data[, `:=`(lon = as.numeric(lon), lat = as.numeric(lat))]
occ_data <- na.omit(occ_data)

# 연구 범위 필터링
ext_ref <- ext(ref_raster)
occ_coords <- occ_data[lon >= ext_ref[1] & lon <= ext_ref[2] & lat >= ext_ref[3] & lat <= ext_ref[4]]
rm(occ_data); gc()

# 5. 래스터화 (각 격자별 점 개수 세기)
# terra::rasterize는 지도를 쪼개서 처리하므로 122GB 에러를 피할 수 있습니다.
cat("지도를 쪼개서 포인트 카운트 수행 중...\n")
pts <- vect(as.matrix(occ_coords), crs = crs(ref_raster))
# 격자 안에 포함된 점의 개수를 직접 계산 (Background = 0)
count_raster <- rasterize(pts, ref_raster, fun = "count", background = 0)
rm(pts, occ_coords); gc()

# 6. 커널 밀도 스무딩 (KDE 효과 구현)
# focal 함수를 사용하여 주변 격자의 값을 가중치로 합산(Gaussian blur 효과)
# sigma(d) 값은 연구의 편향 보정 범위를 결정합니다. (예: 해상도의 20배 거리)
cat("커널 스무딩(KDE 시뮬레이션) 수행 중... 이 작업은 시간이 걸립니다.\n")
# 가우시안 커널 생성 (해상도 기준 약 20~50격자 범위 스무딩)
w <- focalMat(count_raster, d = res(count_raster)[1] * 20, type = 'Gauss')
bias_raster <- focal(count_raster, w = w, fun = sum, na.rm = TRUE)
rm(count_raster); gc()

# 7. 마스킹 및 정규화
cat("최종 정규화 작업 중...\n")
bias_raster <- mask(bias_raster, ref_raster)

# 1 ~ 1000 정규화 (Min-Max)
v_min <- global(bias_raster, "min", na.rm = TRUE)[1,1]
v_max <- global(bias_raster, "max", na.rm = TRUE)[1,1]
bias_final <- ((bias_raster - v_min) / (v_max - v_min) * 999) + 1

# 육지(데이터가 있는 곳)인데 결측치인 곳은 최소값 1로 채움
bias_final[is.na(bias_final) & !is.na(ref_raster)] <- 1

# 8. 최종 저장 (.asc)
cat("결과 저장 중...\n")
writeRaster(bias_final, file.path(base_dir, "bias_file_full_res.asc"), 
            overwrite = TRUE, NAflag = -9999)
