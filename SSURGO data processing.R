install.packages("modeest")

library(sf)
library(terra)
library(dplyr)
library(modeest)  # mode 계산용

# 경로
gdb_path <- "C:/2025independentstudy/input/ssurgo_unzip/gSSURGO_NY.gdb"
output_dir <- "C:/2025independentstudy/output_asc"
dir.create(output_dir, showWarnings = FALSE)

# 읽을 변수
numeric_vars <- c("slopegraddcp", "ecec_r", "sumbases_r", "silttotal_r", "claytotal_r", "caco3_r", "flodfreqdcd", "flodfreqmax", "forpehrtdcp")
all_vars <- c(numeric_vars)
#----------------------------------------
# GDB 경로
gdb_path <- "C:/2025independentstudy/input/ssurgo_unzip/gSSURGO_NY.gdb"

# 공간정보 있는 폴리곤 (mukey 포함)
mu_poly <- st_read(gdb_path, layer = "MUPOLYGON", quiet = TRUE)

# 속성 테이블 (mukey 기준으로 조인할 데이터)
muaggatt <- st_read(gdb_path, layer = "muaggatt", quiet = TRUE)
component <- st_read(gdb_path, layer = "component", quiet = TRUE)
chorizon <- st_read(gdb_path, layer = "chorizon", quiet = TRUE)

# 필요한 열만 남기기
vars_needed <- c("mukey", "slopegraddcp", "flodfreqdcd", "flodfreqmax", 
                 "forpehrtdcp", "ecec_r", "sumbases_r", 
                 "silttotal_r", "claytotal_r", "caco3_r")

# 필요한 열만 선택
muaggatt_sel <- muaggatt %>%
  select(mukey, slopegraddcp, flodfreqdcd, flodfreqmax, forpehrtdcp)

component_sel <- component %>%
  select(mukey, cokey)

chorizon_sel <- chorizon %>%
  select(cokey, ecec_r, sumbases_r, silttotal_r, claytotal_r, caco3_r)

# 데이터 통합
mu_data <- muaggatt_sel %>%
  left_join(component_sel, by = "mukey") %>%
  left_join(chorizon_sel, by = "cokey")

safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# 각 mukey별 요약 통계 계산
# - numeric 변수는 평균값(mean), categorical 변수는 최빈값(mode)
summary_by_mukey <- mu_data %>%
  group_by(mukey) %>%
  summarise(
    slopegraddcp = mean(slopegraddcp, na.rm = TRUE),
    ecec_r = mean(ecec_r, na.rm = TRUE),
    sumbases_r = mean(sumbases_r, na.rm = TRUE),
    silttotal_r = mean(silttotal_r, na.rm = TRUE),
    claytotal_r = mean(claytotal_r, na.rm = TRUE),
    caco3_r = mean(caco3_r, na.rm = TRUE),
    flodfreqdcd = safe_mode(flodfreqdcd),
    flodfreqmax = safe_mode(flodfreqmax),
    forpehrtdcp = safe_mode(forpehrtdcp)
  )


# 공간 데이터와 결합
mu_poly_joined <- mu_poly %>%
  left_join(summary_by_mukey, by = c("MUKEY" = "mukey"))


library(sp)

for (var in numeric_vars) {
  message("Processing: ", var)
  
  # 해당 변수만 추출
  var_sf <- mu_poly_joined %>%
    select(all_of(var))
  
  # NA 값 제거 (terra::rasterize는 NA geometry를 무시)
  var_sf <- var_sf[!is.na(var_sf[[var]]), ]
  
  # sf 객체를 sp 객체로 변환
  var_sp <- as(var_sf, "Spatial")
  
  # 기준 해상도 설정 (예: 100m)
  r_template <- rast(ext(var_sp), resolution = 100, crs = st_crs(var_sf)$wkt)
  
  # 래스터화
  r <- terra::rasterize(var_sp, r_template, field = var, fun = mean)
  
  # 저장
  writeRaster(r, filename = file.path(output_dir, paste0(var, ".asc")), overwrite = TRUE)
}



#-------------------------------------------기
library(sf)
library(dplyr)
library(terra)

# 경로 설정
gdb_path <- "C:/2025independentstudy/input/ssurgo_unzip/gSSURGO_NY.gdb"
output_dir <- "C:/2025independentstudy/output_asc"
dir.create(output_dir, showWarnings = FALSE)

# 필요한 변수 설정
chorizon_numeric <- c("ecec_r", "sumbases_r", "silttotal_r", "claytotal_r", "caco3_r")
muaggatt_numeric <- c("slopegraddcp")
categorical_vars <- c("flodfreqdcd", "flodfreqmax", "forpehrtdcp")  # 변수명 재확인
numeric_vars <- c(muaggatt_numeric, chorizon_numeric)

# 최빈값 함수
safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 1. chorizon
message("Loading chorizon...")
chorizon <- st_read(gdb_path, layer = "chorizon", quiet = TRUE)
chorizon_sel <- chorizon %>%
  select(cokey, all_of(chorizon_numeric)) %>%
  group_by(cokey) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")
rm(chorizon); gc()

# 2. component
message("Loading component...")
component <- st_read(gdb_path, layer = "component", quiet = TRUE)
component_sel <- component %>%
  select(mukey, cokey)
rm(component); gc()

# 3. muaggatt
message("Loading muaggatt...")
muaggatt <- st_read(gdb_path, layer = "muaggatt", quiet = TRUE)
muaggatt_sel <- muaggatt %>%
  select(mukey, all_of(muaggatt_numeric), all_of(categorical_vars))
rm(muaggatt); gc()

# 4. 데이터 통합
message("Joining tables...")
mu_data <- component_sel %>%
  left_join(chorizon_sel, by = "cokey") %>%
  left_join(muaggatt_sel, by = "mukey") %>%
  group_by(mukey) %>%
  summarise(
    across(all_of(numeric_vars), ~mean(.x, na.rm = TRUE)),
    across(all_of(categorical_vars), ~safe_mode(.x)),
    .groups = "drop"
  )
rm(chorizon_sel, component_sel, muaggatt_sel); gc()

# 5. MUPOLYGON 불러오기
message("Loading MUPOLYGON...")
mu_poly <- st_read(gdb_path, layer = "MUPOLYGON", quiet = TRUE)
mu_poly_joined <- mu_poly %>%
  left_join(mu_data, by = c("MUKEY" = "mukey"))
rm(mu_poly); gc()

# 6. 래스터화 및 asc 저장 (청크 없이 전체 처리)
message("Rasterizing numeric variables without chunking...")

# 벡터 객체로 변환
mu_vect <- terra::vect(mu_poly_joined)

# 전체 범위 기준 래스터 템플릿 생성 (100m 해상도)
r_template <- rast(ext(mu_vect), resolution = 100, crs = crs(mu_vect))

# 변수별 처리
for (i in seq_along(numeric_vars)) {
  var <- numeric_vars[i]
  message(sprintf("\n[%d/%d] Processing: %s", i, length(numeric_vars), var))
  iter_start <- Sys.time()
  
  # NA 제거
  temp_vect <- mu_vect[!is.na(mu_vect[[var]]), ]
  if (nrow(temp_vect) == 0) {
    message("  ▶ 건너뜀 (모든 값이 NA)")
    next
  }
  
  # 래스터화 (mean)
  r <- terra::rasterize(temp_vect, r_template, field = var, fun = mean, touches = TRUE)
  
  # 저장
  writeRaster(r, filename = file.path(output_dir, paste0(var, ".asc")), overwrite = TRUE)
  
  # 완료 메시지
  iter_end <- Sys.time()
  message(sprintf("  ▶ 완료 (%.2f초)", as.numeric(difftime(iter_end, iter_start, units = "secs"))))
  
  # 메모리 해제
  rm(temp_vect, r); gc()
}